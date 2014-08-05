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
    opts <- fmap parseopts getArgs
    brain <- emptyBrain
    let
        pl = plugins brain
        cs = childstatus brain
        mq = messagequeue brain
    case opts of 
        Left msg -> do  
            hPutStr stderr msg
            exitFailure
        Right o  -> bracket 
            (startPluginThread pl cs mq >>= connect o pl cs)
            (hClose . socket) 
            ((flip $ runbot brain) run)

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
    [ "!hauth <password>: authenticate with harkerbot"
    , "!help:             this dialog"
    , "!hunauth:          unauthenticate with harkerbot"
    , "!id <statement>:   echo <statement>"
    , "!pingalert:        print a message in the default"
    , "                   channel when a ping is recieved"
    , "!plugins:          list all current plugins"
    , "!quit:             force harkerbot to shutdown"
    , "!unplug:           remove a plugin"
    , "!uptime:           show how long harkerbot has been"
    , "                   running"
    ]

evalpriv :: IRCInPrivMsg -> Bot ()
evalpriv msg
    | m == "!quit"              = runauth n u c (quitfunc "Exiting")
    | m == "!uptime"            = uptime >>= privmsg n c
    | m == "!honkslam"          = runauth n u c (togglehonkslam n u c)
    | m == "!pingalert"         = runauth n u c (togglepingalert n u c)
    | m == "!hauth"             = privmsg n c "needs a password idiot"
    | m == "!plugins"           = pluginList >>= mapM_ (privmsg n c)
    | m == "!help"              = mapM_ (privmsg n c) helpList
    | "!unplug " `isPrefixOf` m = runauth n u c (unplug (drop 8 m) 
                                                >>= privmsg n c)
    | "!hauth " `isPrefixOf` m  = auth u (drop 7 m) >>= privmsg n c
    | m == "!hunauth"           = runauth n u c (unauth n u c)
    | "!id " `isPrefixOf` m     = privmsg n c (drop 4 m)
    | map toLower m == "honk"   = hslam n c m
    | otherwise                 = pluginBroadcast msg
    where
        n = ircNick msg
        m = ircMsg  msg
        u = ircUser msg
        c = ircChan msg

pluginBroadcast :: IRCInPrivMsg -> Bot ()
pluginBroadcast msg = do
    let mstr = foldl (\a b -> a ++ b ++ "\n") "" (toList msg) ++ "-"
    l <- gets plugins >>= liftIO . readMVar
    liftIO $ mapM_ (\(_,_,h,_) -> sendmsg mstr h) l

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
