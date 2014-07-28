import Control.Applicative
import Control.Arrow
import Control.Exception.Base
import Control.Monad.Reader
import Control.Monad.State
import Data.Char
import Data.List
import Network
import System.Exit
import System.Time
import System.IO
import Text.Printf

type User = String
data BotCore  = BotCore { socket    :: Handle 
                        , starttime :: ClockTime
                        }
data BotBrain = BotBrain { botauth  :: Maybe User
                         , honkslam :: Bool
                         }
type Bot a = ReaderT BotCore (StateT BotBrain IO) a

emptyBrain = BotBrain Nothing False
pass      = "harker"
server    = "segfault.net.nz"
port      = 6667
chan      = "#bots"
nick      = "harker"
honklimit = 7 

main :: IO ()
main = bracket connect 
    (hClose . socket) 
    (\bc -> runStateT (runReaderT run bc) emptyBrain >> return ())

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

write :: String -> String -> Bot ()
write s t = do
    h <- asks socket
    liftIO $ hPrintf h "%s %s\r\n" s t
    liftIO $ printf    "> %s %s\n" s t

listen :: Handle -> Bot ()
listen h = loopfunc $ do
    s <- liftIO $ fmap init (hGetLine h)
    liftIO $ printf "< %s\n" s
    if ping s then pong s else eval (clean s)
    where
        loopfunc a = a >> loopfunc a
        clean x = let msg = drop 1 . dropWhile (/= ':') $ drop 1 x
                      usr = takeWhile (/= '!') $ drop 1 x
                      drp = drop 1 . dropWhile (/= ' ')
                      chn = takeWhile (/= ' ') . drp . drp  $ drop 1 x
                  in  (usr, chn, msg)
        ping = ("PING :" `isPrefixOf`)
        pong = write "PONG" . (':' :) . drop 6 

eval :: (User, String, String) -> Bot ()
eval (u, c, "!quit")     = runauth u c quitfunc 
eval (u, c, "!uptime")   = uptime >>= privmsg u c
eval (u, c, "!hauth")    = privmsg u c "needs a password idiot"
eval (u, c, "!honkslam") = runauth u c (togglehonkslam u c)
eval (u, c, x) 
    | "!id " `isPrefixOf` x    = privmsg u c (drop 4 x)
    | "!hauth " `isPrefixOf` x = auth u (drop 7 x) >>= privmsg u c
    | map toLower x == "honk"  = hslam u c x
    | checkReg x               = ircInit
    | otherwise                = return ()

hslam :: User -> String -> String -> Bot ()
hslam u c x = do
    honk <- gets honkslam
    if honk then sequence_ . take honklimit $ repeat (privmsg u c x)
            else return ()

quitfunc :: Bot ()
quitfunc = write "QUIT" ":Exiting" >> liftIO exitSuccess

togglehonkslam :: ser -> String -> Bot ()
togglehonkslam u c = do
    h <- not <$> gets honkslam
    modify (togglehonk h)
    privmsg u c $ "honkslam is now " ++ if h then "on" else "off"
    where
        togglehonk h x = x { honkslam = h }

privmsg :: User -> String -> String -> Bot ()
privmsg u c s = if head c == '#' then write "PRIVMSG" (c ++ " :" ++ s)
                                 else write "PRIVMSG" (u ++ " :" ++ s)

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
                modify (setauth u)
                return $ "you have successfully authenticated as " ++ u
            else return "incorrect password"

setauth :: User -> BotBrain -> BotBrain
setauth u b = b { botauth = Just u }

isauth :: User -> BotBrain -> Bool
isauth u b = case botauth b of
                Just a -> a == u
                _      -> False

runauth :: User -> String -> Bot () -> Bot ()
runauth u c f = do
    brain <- get
    if isauth u brain then f
                      else privmsg u c "you are not authenticated for that"
