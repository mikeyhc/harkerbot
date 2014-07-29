import Control.Applicative
import Control.Arrow
import Control.Exception.Base
import Control.Monad.Reader
import Control.Monad.State
import Data.Char
import Data.List
import HarkerIRC.Types
import Network
import System.Exit
import System.Time
import System.IO
import Text.Printf

data BotCore  = BotCore { socket    :: Handle 
                        , starttime :: ClockTime
                        }
data BotBrain = BotBrain { botauth   :: Maybe User
                         , honkslam  :: Bool
                         , pingalert :: Bool
                         }
type Bot a = ReaderT BotCore (StateT BotBrain IO) a

runbot :: BotBrain -> BotCore -> IO ()
runbot bb bc = runStateT (runReaderT run bc) bb >> return ()

emptyBrain = BotBrain Nothing False False
pass      = "harker"
server    = "segfault.net.nz"
port      = 6667
chan      = "#bots"
nick      = "harker"
honklimit = 7 

main :: IO ()
main = bracket connect 
    (hClose . socket) 
    (runbot emptyBrain)

connect :: IO BotCore
connect = notify $ do
    t <- getClockTime
    h <- connectTo server (PortNumber (fromIntegral port))
    hSetBuffering h NoBuffering
    return $ BotCore h t
    where
        notify = bracket_
            (printf "Connecting to %s..." server >> hFlush stdout)
            (printf "done.\n")

run :: Bot ()
run = ircInit >> asks socket >>= listen

ircInit :: Bot ()
ircInit = do
    h <- asks socket
    write "NICK" nick
    write "USER" $ nick ++ " 0 * :harker bot"
    write "JOIN" chan

write :: String -> Message -> Bot ()
write s t = do
    h <- asks socket
    liftIO $ hPrintf h "%s %s\r\n" s t
    liftIO $ printf    "> %s %s\n" s t

listen :: Handle -> Bot ()
listen h = loopfunc $ do
    s <- liftIO $ fmap init (hGetLine h)
    liftIO $ printf "< %s\n" s
    if ping s then do 
            pong s
            pa <- gets pingalert
            if pa then write "PRIVMSG " (chan ++ " :ping <---> pong")
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

evalpriv :: IRCInPrivMsg -> Bot ()
evalpriv msg
    | m == "!quit"             = runauth n u c quitfunc 
    | m == "!uptime"           = uptime >>= privmsg n c
    | m == "!honkslam"         = runauth n u c (togglehonkslam n u c)
    | m == "!pingalert"        = runauth n u c (togglepingalert n u c)
    | m == "!hauth"            = privmsg n c "needs a password idiot"
    | "!hauth " `isPrefixOf` m = auth u (drop 7 m) >>= privmsg n c
    | m == "!hunauth"          = runauth n u c (unauth n u c)
    | "!id " `isPrefixOf` m    = privmsg n c (drop 4 m)
    | map toLower m == "honk"  = hslam n c m
    | otherwise                = return ()
    where
        n = ircNick msg
        m = ircMsg  msg
        u = ircUser msg
        c = ircChan msg

evalsys :: IRCSystemMsg -> Bot ()
evalsys msg 
    | checkReg m = ircInit
    | otherwise  = return ()
    where
        m = getIRCSysMsg msg

hslam :: Nick -> Chan -> Message -> Bot ()
hslam n c m = do
    honk <- gets honkslam
    if honk then sequence_ . take honklimit $ repeat (privmsg n c m)
            else return ()

quitfunc :: Bot ()
quitfunc = write "QUIT" ":Exiting" >> liftIO exitSuccess

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
            if p == pass then do
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
