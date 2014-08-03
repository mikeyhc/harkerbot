import Control.Applicative
import Control.Arrow
import Control.Concurrent (forkFinally, MVar, newEmptyMVar, putMVar, takeMVar
                          ,yield, ThreadId, tryTakeMVar, readMVar, newMVar
                          ,threadDelay, modifyMVar_, forkIO)
import Control.Exception
import Control.Exception.Base
import Control.Monad.Reader
import Control.Monad.State
import Data.Char
import Data.List
import Data.Maybe
import Data.Typeable
import HarkerIRC.Types
import HarkerServer.Args
import HarkerServer.Types
import Network
import System.Console.GetOpt
import System.Directory
import System.Environment
import System.Exit
import System.Time
import System.Timeout
import System.IO
import Text.Printf

main :: IO ()
main = do
    opts <- fmap parseopts getArgs
    brain <- emptyBrain
    case opts of 
        Left msg -> do  
            hPutStr stderr msg
            exitFailure
        Right o  -> bracket ( 
                startPluginThread (plugins brain) (childstatus brain)
                >>= connect o (plugins brain) (childstatus brain))
            (hClose . socket) 
            ((flip $ runbot brain) run)

startPluginThread :: MVar [Plugin] -> MVar Status -> IO ThreadId
startPluginThread pl cs = do
        putMVar cs Starting
        tid <- forkFinally (pluginListener pl cs) 
            (\e -> do { case e of { 
                        Left err -> putStrLn $ "Exception: " ++ show e;
                        _        -> return () };
                    closeplugins pl >> takeMVar cs >> putMVar cs Dead })
        v <- readMVar cs
        if isRunning v then return tid
        else hPutStr stderr ("Couldn't open " ++ unixaddr) >> exitFailure

connect :: OptSet -> MVar [Plugin] -> MVar Status -> ThreadId -> IO BotCore
connect (n, pa, s, po, c, m) pl cs tid = notify s po $ do
        t <- getClockTime
        h <- connectTo s (PortNumber (fromIntegral po))
        hSetBuffering h NoBuffering
        return $ BotCore h t tid n pa c m
    where
        notify :: String -> Int -> IO a -> IO a
        notify a b = bracket_
            (printf "Connecting to %s:%d...\n" a b >> hFlush stdout)
            (printf "Connected to %s.\n" a)

pluginListener :: MVar [Plugin] -> MVar Status -> IO ()
pluginListener pl status = do
    printf "Starting Plugin Thread\n"
    bracket (connectUnixPlugin pl status) (sClose . pluginSock) 
            (flip runPluginThread pluginThread)

closeplugins :: MVar [Plugin] -> IO ()
closeplugins pl = takeMVar pl >>= mapM_ (\(_,_,h) -> hClose h) 
                  >> putMVar pl []

unixaddr = "/tmp/.harker-server.sock"
unixSocket = UnixSocket unixaddr

connectUnixPlugin :: MVar [Plugin] -> MVar Status -> IO PluginCore
connectUnixPlugin p s = do
    fe <- doesFileExist unixaddr
    if fe then removeFile unixaddr else return ()
    r <- PluginCore p s <$> listenOn unixSocket
    putStrLn $ unixaddr ++ " opened"
    _ <- takeMVar  s
    putMVar s Running
    return r

pluginThread :: PluginThread ()
pluginThread = do 
    sock <- asks pluginSock
    pl <- asks pluginVar
    (h, hn, _) <- liftIO $ accept sock
    liftIO $ forkFinally (pluginFromHandle h pl) 
        (\e -> do 
            case e of
                Left err -> putStrLn $ "Exception: " ++ show err 
                _        -> return ()
            hClose h)
    pluginThread 

pluginFromHandle :: Handle -> MVar [Plugin] -> IO ()
pluginFromHandle h pl = do
    n <- hGetLine h
    v <- hGetLine h
    s <- hGetLine h
    case parsePluginRegister n v s of 
        Left msg -> hPutStrLn h msg
        Right (n', v', s') -> do
            mh' <- fmap Just (connectTo "localhost" (UnixSocket s')) 
                `catch` handleException
            case mh' of
                Just h' -> do
                    hPutStrLn h "registered" 
                    modifyMVar_ pl 
                        (\l -> return $ (n', v', h'):filter (ffilt n') l)
                Nothing -> do
                    hPutStrLn h $ "could not connect to " ++ s'
                    return ()
    where
        handleException :: SomeException -> IO (Maybe Handle)
        handleException e = do
            putStrLn $ "connection exception: " ++ show e
            return Nothing

        ffilt a (b, _, _) = a /= b

parsePluginRegister :: String -> String -> String 
                    -> Either String (String, String, String)
parsePluginRegister n v s =
    let (hn, nv) = splitAt 6 n
        (vn, vv) = splitAt 9 v
        (sn, sv) = splitAt 8 s
    in   if hn == "name: "    && nv /= []
    then if vn == "version: " && vv /= []
    then if sn == "server: "  && sv /= []
    then Right (nv, vv, sv)
    else Left "no server component"
    else Left "no version component"
    else Left "no name component"

run :: Bot ()
run = ircInit >> asks socket >>= listen

ircInit :: Bot ()
ircInit = do
    n <- asks nick
    c <- asks chan
    write "NICK" n
    write "USER" $ n ++ " 0 * :harker bot"
    write "JOIN" c

write :: String -> Message -> Bot ()
write s t = do
    h <- asks socket
    liftIO $ hPrintf h "%s %s\r\n" s t
    liftIO $ printf    "> %s %s\n" s t

listen :: Handle -> Bot ()
listen h = loopfunc $ do
    cs <- gets childstatus 
    c  <- liftIO $ takeMVar cs
    if not (isRunning c) 
        then quitfunc "Plugin thread died"
        else do
            liftIO $ putMVar cs c
            s <- liftIO $ fmap init (hGetLine h)
            liftIO $ printf "< %s\n" s
            if ping s then do 
                    pong s
                    c  <- asks chan
                    pa <- gets pingalert
                    if pa then write "PRIVMSG " (c ++ " :ping <---> pong")
                          else return ()
                else case parseRawIRC s of
                    Left  a -> evalsys a
                    Right b -> evalpriv b
    where
        loopfunc a = a >> loopfunc a
        ping = ("PING :" `isPrefixOf`)
        pong = write "PONG" . (':' :) . drop 6 

parseRawIRC :: RawIRCString -> Either IRCSystemMsg IRCInPrivMsg
parseRawIRC s = let (n, a) = second tail' . break (== '!') $ tail' s
                    a'     = case a of { '~':xs -> xs; _ -> a }
                    (u, b) = second tail' $ break (== ' ') a'
                    b'     = tail' $ dropWhile (/= ' ') b
                    (c, m) = second (tail' . tail') $ break (== ' ') b'
                    getmsg = tail' . dropWhile (/= ':') . tail'
                in if m == [] then Left  . IRCSystemMsg $ getmsg s
                              else Right $ IRCInPrivMsg n u False c m

tail' :: [a] -> [a]
tail' (_:xs) = xs
tail' _      = []

helpList :: [String]
helpList = 
    [ "!hauth:     authenticate with harkerbot"
    , "!help:      this dialog"
    , "!hunauth:   unauthenticate with harkerbot"
    , "!pingalert: print a message in the default channel when a ping \
        \is recieved"
    , "!plugins:   list all current plugins"
    , "!quit:      force harkerbot to shutdown"
    , "!unplug:    remove a plugin"
    , "!uptime:    show how long harkerbot has been running"
    ]

evalpriv :: IRCInPrivMsg -> Bot ()
evalpriv msg
    | m == "!quit"             = runauth n u c (quitfunc "Exiting")
    | m == "!uptime"           = uptime >>= privmsg n c
    | m == "!honkslam"         = runauth n u c (togglehonkslam n u c)
    | m == "!pingalert"        = runauth n u c (togglepingalert n u c)
    | m == "!hauth"            = privmsg n c "needs a password idiot"
    | m == "!plugins"          = pluginList >>= mapM_ (privmsg n c)
    | m == "!help"             = mapM_ (privmsg n c) helpList
    | "!hauth " `isPrefixOf` m = auth u (drop 7 m) >>= privmsg n c
    | m == "!hunauth"          = runauth n u c (unauth n u c)
    | "!id " `isPrefixOf` m    = privmsg n c (drop 4 m)
    | map toLower m == "honk"  = hslam n c m
    | otherwise                = pluginBroadcast msg
    where
        n = ircNick msg
        m = ircMsg  msg
        u = ircUser msg
        c = ircChan msg

pluginBroadcast :: IRCInPrivMsg -> Bot ()
pluginBroadcast msg = do
    let mstr = foldl (\a b -> a ++ b ++ "\n") "" $ toList msg
    l <- gets plugins >>= liftIO . readMVar
    liftIO $ mapM_ (\(_,_,h) -> sendmsg mstr h) l

sendmsg :: String -> Handle -> IO ThreadId
sendmsg str h = forkIO $ hPutStrLn h str 

evalsys :: IRCSystemMsg -> Bot ()
evalsys msg 
    | checkReg m = ircInit
    | otherwise  = return ()
    where
        m = getIRCSysMsg msg

hslam :: Nick -> Chan -> Message -> Bot ()
hslam n c m = do
    honk <- gets honkslam
    ml <- asks maxlines
    if honk then sequence_ . take ml $ repeat (privmsg n c m)
            else return ()

quitfunc :: Message -> Bot ()
quitfunc s = write "QUIT" (":" ++ s) >> liftIO exitSuccess

togglehonkslam :: Nick -> User -> Chan -> Bot ()
togglehonkslam n u c = do
    h <- not <$> gets honkslam
    modify (togglehonk h)
    privmsg n c $ "honkslam is now " ++ if h then "on" else "off"
    where
        togglehonk h x = x { honkslam = h }

togglepingalert :: Nick -> User -> Chan -> Bot ()
togglepingalert n u c = do
    p <- not <$> gets pingalert
    modify (toggleping p)
    privmsg n c $ "pingalert is now " ++ if p then "on" else "off"
    where
        toggleping p x = x { pingalert = p }

privmsg :: Nick -> Chan -> Message -> Bot ()
privmsg n c s = if head c == '#' then write "PRIVMSG" (c ++ " :" ++ s)
                                 else write "PRIVMSG" (n ++ " :" ++ s)

checkReg :: String -> Bool
checkReg = (==) "You have not registered"

uptime :: Bot String
uptime = do
    now  <- liftIO getClockTime
    zero <- asks starttime
    return . prettyClockDiff $ diffClockTimes now zero

pluginList :: Bot [String]
pluginList = do
    l <- gets plugins >>= liftIO . readMVar
    return $ header:map (\(n,v,_) -> "    " ++ n ++ ": v" ++ v) l
    where
        header = "registered plugins:"

prettyClockDiff :: TimeDiff -> String
prettyClockDiff td =
    unwords $ map (uncurry (++) . first show) $
    if null diffs then [(0, "s")] else diffs
    where
        merge (tot, acc) (sec, typ) = let (sec', tot') = divMod tot sec
                                      in  (tot', (sec', typ):acc)
        metrics = [(86400,"d"), (3600,"h"), (60, "m"), (1, "s")]
        diffs = filter ((/= 0) . fst) $ reverse $ snd $
                foldl' merge (tdSec td, []) metrics

auth :: User -> String -> Bot String
auth u p = do
    ma <- gets botauth
    case ma of
        Just a -> return "somebody has already authenticated"
        _      -> do
            p' <- asks pass
            if p == p' then do
                modify (setauth $ Just u)
                return $ "you have successfully authenticated as " ++ u
            else return "incorrect password"

setauth :: Maybe User -> BotBrain -> BotBrain
setauth u b = b { botauth = u }

isauth :: User -> BotBrain -> Bool
isauth u b = case botauth b of
                Just a -> a == u
                _      -> False

unauth :: Nick -> User -> Chan -> Bot ()
unauth n u c = do
    modify (setauth Nothing)
    privmsg n c "you have unauthenticated"

runauth :: Nick -> User -> Chan -> Bot () -> Bot ()
runauth n u c f = do
    brain <- get
    if isauth u brain then f
                      else privmsg n c "you are not authenticated for that"
