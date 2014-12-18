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
import HarkerServer.Plugin
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
    fe <- doesFileExist unixaddr
    if fe then hPutStrLn stderr "Server currently running (or stale socket)"
          else ircConnect `catch` cleanup
  where
    cleanup :: SomeException -> IO ()
    cleanup e = do
        hPrint stderr e
        fe <- doesFileExist unixaddr
        when fe $ removeFile unixaddr

ircConnect :: IO ()
ircConnect = do
    opts <- fmap parseopts getArgs
    brain <- emptyBrain
    let pl = plugins brain
        cs = childstatus brain
        mq = messagequeue brain
    case opts of 
        Left msg -> hPutStr stderr msg >> exitFailure
        Right o  -> do
            ptid <- startPluginThread pl cs mq (optSetPlugDir o) 
            core <- connect o pl cs ptid 
            runFunc brain core `catch` reconnect brain core o pl cs ptid
            hClose $ socket core
  where
    reconnect :: BotBrain -> BotCore -> OptSet -> MVar [Plugin] -> MVar Status
              -> ThreadId -> SomeException -> IO ()
    reconnect brain core o pl cs ptid exn = do
        hPrint stderr exn
        threadDelay 1000000
        hClose $ socket core
        newcore <- connect o pl cs ptid
        let newcore' = newcore { starttime = starttime core }
        runFunc brain newcore' `catch` reconnect brain newcore' o pl cs ptid

runFunc :: BotBrain -> BotCore -> IO ()
runFunc brain core = runbot brain core run

connect :: OptSet -> MVar [Plugin] -> MVar Status -> ThreadId -> IO BotCore
connect (OptSet n pa s po c m _) pl cs tid = notify s po $ do
        t <- getClockTime
        h <- connectTo s (PortNumber (fromIntegral po))
        hSetBuffering h NoBuffering
        return $ BotCore h t tid n pa c m
    where
        notify :: String -> Int -> IO a -> IO a
        notify a b = bracket_
            (printf "Connecting to %s:%d...\n" a b >> hFlush stdout)
            (printf "Connected to %s.\n" a)

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
write s m = do
    h <- asks socket
    liftIO $ hWrite h s m

hWrite :: Handle -> String -> Message -> IO ()
hWrite h s m = do
    hPrintf h "%s %s\r\n" s m
    printf    "> %s %s\n" s m

listen :: Handle -> Bot ()
listen h = do
    v <- gets messagequeue
    liftIO $ hSetEncoding stdout utf8
    liftIO . forkIO $ echoThread h v
    loopfunc $ do
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
                        when pa $ write "PRIVMSG " (c ++ " :ping <---> pong")
                    else do
                        emsg <- parseRawIRC s 
                        case emsg of
                            Left  a -> evalsys a
                            Right b -> do   
                                il <- gets ignorelist
                                unless (isJust $ find (== ircNick b) il) 
                                    $ evalpriv b
    where
        loopfunc a = a >> loopfunc a
        ping = ("PING :" `isPrefixOf`)
        pong = write "PONG" . (':' :) . drop 6 

echoThread :: Handle -> MVar OutMessageQueue -> IO ()
echoThread h m = loopfunc $ do
    l <- liftIO $ takeMVar m
    mapM_ (\x -> hPrivmsg h (_outircnick x)
                            (_outircchan x)
                            (_outircmsg  x)) l
    liftIO $ putMVar m []
    

parseRawIRC :: RawIRCString -> Bot (Either IRCSystemMsg IRCInPrivMsg)
parseRawIRC s = let (n, a) = second tail' . break (== '!') $ tail' s
                    a'     = case a of { '~':xs -> xs; _ -> a }
                    (u, b) = second tail' $ break (== ' ') a'
                    b'     = tail' $ dropWhile (/= ' ') b
                    (c, m) = second (tail' . tail') $ break (== ' ') b'
                    getmsg = tail' . dropWhile (/= ':') . tail'
                in if null m then return . Left  . IRCSystemMsg $ getmsg s
                             else do
                                au <- checkAuth u
                                nick <- asks nick
                                return . Right $ IRCInPrivMsg n nick u au c m

checkAuth :: User -> Bot Bool
checkAuth user = do
    mu <- gets botauth
    case mu of
        Just u -> return $ u == user
        _      -> return False

tail' :: [a] -> [a]
tail' (_:xs) = xs
tail' _      = []

helpList :: [String]
helpList = 
    [ "!hauth <password>: authenticate with harkerbot"
    , "!help:             this dialog"
    , "!help <plugin>:    get help on a plugin"
    , "                   (if available)"
    , "!hunauth:          unauthenticate with harkerbot"
    , "!id <statement>:   echo <statement>"
    , "!ignore <nick>:    ignore messages from user"
    , "!pingalert:        print a message in the default"
    , "                   channel when a ping is recieved"
    , "!plugins:          list all current plugins"
    , "!quit:             force harkerbot to shutdown"
    , "!unignore <nick>:  start listening to user again"
    , "!unplug:           remove a plugin"
    , "!uptime:           show how long harkerbot has been"
    , "                   running"
    ]

evalpriv :: IRCInPrivMsg -> Bot ()
evalpriv msg
    | m == "!quit"                = runauth n u c (quitfunc "Exiting")
    | m == "!uptime"              = uptime >>= privmsg n c
    | m == "!pingalert"           = runauth n u c (togglepingalert n u c)
    | m == "!hauth"               = privmsg n c "needs a password idiot"
    | m == "!plugins"             = pluginList >>= mapM_ (privmsg n c)
    | m == "!help"                = mapM_ (privmsg n c) helpList
    | "!help " `isPrefixOf` m     = pluginHelp msg (drop 6 m)
    | "!unplug " `isPrefixOf` m   = runauth n u c (unplug c (drop 8 m)
                                                  >>= \x -> maybe (return ())
                                                            (privmsg n c) x)
    | "!hauth " `isPrefixOf` m    = auth u (drop 7 m) >>= privmsg n c
    | m == "!hunauth"             = runauth n u c (unauth n u c)
    | "!id " `isPrefixOf` m       = privmsg n c (drop 4 m)
    | "!ignore " `isPrefixOf` m   = runauth n u c (ignore (drop 8 m)
                                                  >>= privmsg n c)
    | "!unignore " `isPrefixOf` m = runauth n u c (unignore (drop 10 m)
                                                  >>= privmsg n c)
    | otherwise                   = pluginBroadcast msg
    where
        n = ircNick msg
        m = ircMsg  msg
        u = ircUser msg
        c = ircChan msg

-- TODO: remove possibly dead threads
pluginBroadcast :: IRCInPrivMsg -> Bot ()
pluginBroadcast msg = do
    let mstr = foldl (\a b -> a ++ b ++ "\n") "" (toList msg) ++ "-"
    l <- gets plugins >>= liftIO . readMVar
    liftIO $ mapM_ (mapfunc mstr) l
  where
    mapfunc :: String -> Plugin -> IO ()
    mapfunc mstr x = void (sendmsg mstr (pluginHandle x))
                `catch` mapExceptHandler (pluginName x)

    mapExceptHandler :: String -> SomeException -> IO ()
    mapExceptHandler name _ = hPutStrLn stderr $ "plugin: " ++ name 
                                                 ++ " appears to be dead"

pluginHelp :: IRCInPrivMsg -> String -> Bot ()
pluginHelp msg name = do
    let n = ircNick msg
        c = ircChan msg
    l <- gets plugins >>= liftIO . readMVar
    case find (\p -> pluginName p == name) l of
        Just p -> do
            let mstr = foldl (\a b -> a ++ b ++ "\n") "" 
                       (toList $ msg { _inircmsg = "!help" }) ++ "-"
            void . liftIO $ sendmsg mstr (pluginHandle p)
        _                 -> privmsg n c $ "No plugin " ++ name

sendmsg :: String -> Handle -> IO ThreadId
sendmsg str h = forkIO $ hPutStrLn h str 

evalsys :: IRCSystemMsg -> Bot ()
evalsys msg 
    | checkReg m = ircInit
    | otherwise  = return ()
  where
    m = getIRCSysMsg msg

quitfunc :: Message -> Bot ()
quitfunc s = do 
    write "QUIT" (":" ++ s) 
    m <- gets plugins
    tid <- asks plugintid
    liftIO . throwTo tid $ ShutdownException Nothing
    liftIO $ putStrLn "waiting for plugins to disconnect"
    liftIO . loopfunc $ do
        r <- readMVar m
        when (null r) exitSuccess

togglepingalert :: Nick -> User -> Chan -> Bot ()
togglepingalert n u c = do
    p <- not <$> gets pingalert
    modify (toggleping p)
    privmsg n c $ "pingalert is now " ++ if p then "on" else "off"
  where
    toggleping p x = x { pingalert = p }

privmsg :: Nick -> Chan -> Message -> Bot ()
privmsg n c s = write "PRIVMSG" $ if head c == '#' then c ++ " :" ++ s
                                                   else n ++ " :" ++ s

hPrivmsg :: Handle -> Nick -> Chan -> Message -> IO ()
hPrivmsg h n c s = hWrite h "PRIVMSG" $ if head c == '#' then c ++ " :" ++ s
                                                         else n ++ " :" ++ s

checkReg :: String -> Bool
checkReg = (==) "You have not registered"

ignore :: Nick -> Bot String
ignore n = do
    il <- gets ignorelist
    case find (== n) il of
        Nothing -> modify (addIgnore il) >> return "user ignored"
        _       -> return "user already ignored"
  where
    addIgnore il x = x { ignorelist = n:il }

unignore :: Nick -> Bot String
unignore n = do
    il <- gets ignorelist
    case find (== n) il of
        Nothing -> return "user not ignored"
        _       -> modify (rmIgnore il) >> return "user unignored"
  where
    rmIgnore il x = x { ignorelist = filter (/= n) il }

uptime :: Bot String
uptime = do
    now  <- liftIO getClockTime
    zero <- asks starttime
    return . prettyClockDiff $ diffClockTimes now zero

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
