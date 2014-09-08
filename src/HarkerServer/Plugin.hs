module HarkerServer.Plugin where

import Control.Applicative ((<$>))
import Control.Arrow
import Control.Concurrent (forkFinally, MVar, newEmptyMVar, putMVar, takeMVar
                          ,yield, ThreadId, tryTakeMVar, readMVar, newMVar
                          ,threadDelay, modifyMVar_, forkIO)
import Control.Exception
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans
import Data.Maybe
import Debug.Trace
import HarkerIRC.Types
import HarkerServer.Types
import Network
import System.Directory
import System.Exit
import System.IO
import Text.Printf

closeplugins :: MVar [Plugin] -> IO ()
closeplugins pl = readMVar pl 
                  >>= mapM_ (\p -> throwTo (pluginThreadId p) 
                                           (ShutdownException ""))

startPluginThread :: MVar [Plugin] -> MVar Status -> MVar OutMessageQueue 
                  -> IO ThreadId
startPluginThread pl cs mq = do
        putMVar cs Starting
        tid <- forkFinally (pluginListener pl mq cs) 
            (\_ -> putStrLn "Stopping plugin thread" >> closeplugins pl 
                   >> takeMVar cs >> putMVar cs Dead)
        v <- readMVar cs
        if isRunning v then return tid
        else hPutStr stderr ("Couldn't open " ++ unixaddr) >> exitFailure

pluginListener :: MVar [Plugin] -> MVar OutMessageQueue -> MVar Status 
               -> IO ()
pluginListener pl mq status = do
    printf "Starting Plugin Thread\n"
    bracket (connectUnixPlugin pl status mq) (sClose . pluginSock) 
            (`runPluginThread` pluginThread)

unixaddr = "/tmp/.harker-server.sock"
unixSocket = UnixSocket unixaddr

connectUnixPlugin :: MVar [Plugin] -> MVar Status -> MVar OutMessageQueue 
                  -> IO PluginCore
connectUnixPlugin p s mq = do
    fe <- doesFileExist unixaddr
    when fe $ removeFile unixaddr 
    r <- PluginCore p s mq <$> listenOn unixSocket
    putStrLn $ unixaddr ++ " opened"
    _ <- takeMVar  s
    putMVar s Running
    return r

pluginFromHandle :: Handle -> MVar [Plugin] -> MVar OutMessageQueue -> IO ()
pluginFromHandle h pl mq = do
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
                    tid <- forkFinally (listenerThread h' mq) 
                                       (shutdownHandler n' pl mq)
                    hPutStrLn h "registered"
                    modifyMVar_ pl 
                        (\l -> return $ Plugin n' v' h' tid
                                      :filter (ffilt n') l)
                Nothing -> do
                    hPutStrLn h $ "could not connect to " ++ s'
                    return ()
    where
        handleException :: SomeException -> IO (Maybe Handle)
        handleException e = do
            putStrLn $ "connection exception: " ++ show e
            return Nothing

        ffilt a p = a /= pluginName p

listenerThread :: Handle -> MVar OutMessageQueue -> IO ()
listenerThread h mq = loopfunc $ do
    s  <- getMore h
    modifyMVar_ mq (return . (++ [fromList s]))
    
getMore :: Handle -> IO [String]
getMore h = do
    s <- hGetLine h
    putStrLn $ "b< " ++ s
    if s == "-" then return []
                else (s:) <$> getMore h

loopfunc a = a >> threadDelay 500 >> loopfunc a

shutdownHandler :: (Exception a) => String -> MVar [Plugin] 
                -> MVar OutMessageQueue -> Either a () -> IO()
shutdownHandler n pl mq e = do
    l <- takeMVar pl
    let (el, l') = removeElement (\x -> n == pluginName x) l 
    putMVar pl l'
    case el of {
        Just p -> let h = pluginHandle p
                  in  putStrLn ("sending quit to " ++ pluginName p) 
                      >> hPutStrLn h "action: quit" 
                      >> hIsOpen h >>= \isOpen -> when isOpen $ hClose h;
        _                 -> return () }
    case e of
        Left err -> 
            if checkException err (ShutdownException "") then do
                msg <- case el of {
                    Just _ -> do {
                        putStrLn $ n ++ " successfully unpluged";
                        mkMessage err $ n ++ " successfully unpluged" };
                    _      -> do {
                        putStrLn $ "could not find plugin " ++ n;
                        mkMessage err "Could not find plugin" } }
                modifyMVar_ mq (return . (++ [msg]))
            else putStrLn $ n ++ " exception: " ++ show err
        Right _  -> putStrLn $ n ++ " exited unexpectedly"
    where
        checkException e r = show e == show r
        mkMessage x = return . IRCOutPrivMsg "#bots" "#bots" -- fix this

removeElement :: (a -> Bool) -> [a] -> (Maybe a, [a])
removeElement f []     = (Nothing, [])
removeElement f (x:xs) = if f x then (Just x, xs)
                                 else second (x:) $ removeElement f xs

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

pluginList :: Bot [String]
pluginList = do
    l <- gets plugins >>= liftIO . readMVar
    return $ header:map (\x -> "    " ++ pluginName x ++ ": v" 
                                      ++  pluginVersion x) l
    where
        header = "registered plugins:"

pluginThread :: PluginThread ()
pluginThread = do 
    sock <- asks pluginSock
    pl <- asks pluginVar
    mq <- asks pluginMsgQueue
    (h, hn, _) <- liftIO $ accept sock
    liftIO $ forkFinally (pluginFromHandle h pl mq) 
        (\e -> do 
            case e of
                Left err -> putStrLn $ "Exception: " ++ show err 
                _        -> return ()
            hClose h)
    pluginThread 

unplug :: Chan -> String -> Bot (Maybe String)
unplug c n = do
        t      <- gets plugins
        plist  <- liftIO $ readMVar t
        liftIO (unplug' n plist)
        where
            unplug' :: String -> [Plugin] -> IO (Maybe String)
            unplug' n []                = return . Just $ failmsg n
            unplug' n (x:xs)
                | n == pluginName x     =
                    throwTo (pluginThreadId x) (ShutdownException c) 
                    >> return Nothing
                | otherwise = unplug' n xs

            failmsg = ("Plugin " ++) . (++ " not found!")
