import Control.Applicative
import Control.Arrow
import Control.Exception.Base
import Control.Monad.Reader
import Control.Monad.State
import Data.Char
import Data.List
import HarkerIRC.Types
import Network
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.Time
import System.IO
import Text.Printf

data CmdFlag 
    = SetNick String
    | SetPass String
    | SetServer String
    | SetPort String
    | SetChan String
    | SetMaxLine String

data BotCore  = BotCore { socket    :: Handle 
                        , starttime :: ClockTime
                        , nick      :: String
                        , pass      :: String
                        , chan      :: String
                        , maxlines  :: Int
                        }
data BotBrain = BotBrain { botauth   :: Maybe User
                         , honkslam  :: Bool
                         , pingalert :: Bool
                         }
type Bot a = ReaderT BotCore (StateT BotBrain IO) a
type OptSet = (String, String, String, Int, String, Int)

runbot :: BotBrain -> BotCore -> IO ()
runbot bb bc = runStateT (runReaderT run bc) bb >> return ()

emptyBrain = BotBrain Nothing False False
defpass      = "harker"
defserver    = "segfault.net.nz"
defport      = 6667
defchan      = "#bots"
defnick      = "harker"
defmaxlines  = 7

options = [Option ['n'] []  (ReqArg SetNick "NICK")
            "Set the default nick for the bot"
          ,Option ['p'] []  (ReqArg SetPass "PASS")
            "Set the authentication password"
          ,Option ['H'] []  (ReqArg SetServer "HOST")  
            "Sets the host to connect to"
          ,Option ['P'] []  (ReqArg SetPort "PORT")
            "Sets the port to connect on"
          ,Option ['c'] []  (ReqArg SetChan "CHANNEL")
            "Sets the channel to connect to"
          ,Option ['m'] []  (ReqArg SetMaxLine "MAXLINES") 
            "Set the maximum lines printed in a public channel"
          ]

emptyOptSet :: OptSet
emptyOptSet = (defnick, defpass, defserver, defport, defchan, defmaxlines)

parseopts :: [String] -> Either String OptSet
parseopts argv = case getOpt Permute options argv of 
    (args, _, []  ) -> Right $ foldr parsearg emptyOptSet args
    (_,    _, errs) -> Left $ concat errs ++ usageInfo header options
    where header = "Usage: harker-server [OPTIONS..]"

_first  g (a, b, c, d, e, f) = (g a, b, c, d, e, f)
_second g (a, b, c, d, e, f) = (a, g b, c, d, e, f)
_third  g (a, b, c, d, e, f) = (a, b, g c, d, e, f)
_fourth g (a, b, c, d, e, f) = (a, b, c, g d, e, f)
_fifth  g (a, b, c, d, e, f) = (a, b, c, d, g e, f)
_sixth  g (a, b, c, d, e, f) = (a, b, c, d, e, g f)

parsearg :: CmdFlag -> OptSet -> OptSet
parsearg (SetNick x)    = _first  (const x)
parsearg (SetPass x)    = _second (const x)
parsearg (SetServer x)  = _third  (const x)
parsearg (SetPort x)    = _fourth (const $ read x)
parsearg (SetChan x)    = _fifth  (const x)
parsearg (SetMaxLine x) = _sixth  (const $ read x)
                             
main :: IO ()
main = do
    opts <- fmap parseopts getArgs
    case opts of 
        Left msg -> do  
            hPutStr stderr msg
            exitFailure
        Right o  -> bracket (connect o)
            (hClose . socket) 
            (runbot emptyBrain)

connect :: OptSet -> IO BotCore
connect (n, pa, s, po, c, m) = notify s po $ do
        t <- getClockTime
        h <- connectTo s (PortNumber (fromIntegral po))
        hSetBuffering h NoBuffering
        return $ BotCore h t n pa c m
    where
        notify :: String -> Int -> IO a -> IO a
        notify a b = bracket_
            (printf "Connecting to %s:%d..." a b >> hFlush stdout)
            (printf "done.\n")

run :: Bot ()
run = ircInit >> asks socket >>= listen

ircInit :: Bot ()
ircInit = do
    h <- asks socket
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
    ml <- asks maxlines
    if honk then sequence_ . take ml $ repeat (privmsg n c m)
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
