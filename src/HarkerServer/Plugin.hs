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
closeplugins pl = takeMVar pl >>= mapM_ (\(_,_,h,_) -> hClose h) 
                  >> putMVar pl []

startPluginThread :: MVar [Plugin] -> MVar Status -> MVar OutMessageQueue 
                  -> IO ThreadId
startPluginThread pl cs mq = do
        putMVar cs Starting
        tid <- forkFinally (pluginListener pl mq cs) 
            (\e -> do { case e of { 
                        Left err -> putStrLn $ "Exception: " ++ show e;
                        _        -> return () };
                    closeplugins pl >> takeMVar cs >> putMVar cs Dead })
        v <- readMVar cs
        if isRunning v then return tid
        else hPutStr stderr ("Couldn't open " ++ unixaddr) >> exitFailure

pluginListener :: MVar [Plugin] -> MVar OutMessageQueue -> MVar Status 
               -> IO ()
pluginListener pl mq status = do
    printf "Starting Plugin Thread\n"
    bracket (connectUnixPlugin pl status mq) (sClose . pluginSock) 
            (flip runPluginThread pluginThread)

unixaddr = "/tmp/.harker-server.sock"
unixSocket = UnixSocket unixaddr

connectUnixPlugin :: MVar [Plugin] -> MVar Status -> MVar OutMessageQueue 
                  -> IO PluginCore
connectUnixPlugin p s mq = do
    fe <- doesFileExist unixaddr
    if fe then removeFile unixaddr else return ()
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
                        (\l -> return $ (n', v', h', tid)
                                      :filter (ffilt n') l)
                Nothing -> do
                    hPutStrLn h $ "could not connect to " ++ s'
                    return ()
    where
        handleException :: SomeException -> IO (Maybe Handle)
        handleException e = do
            putStrLn $ "connection exception: " ++ show e
            return Nothing

        ffilt a (b, _, _, _) = a /= b

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

loopfunc a = a >> loopfunc a

shutdownHandler :: (Exception a) => String -> MVar [Plugin] 
                -> MVar OutMessageQueue -> Either a () -> IO()
shutdownHandler n pl mq e = do
    l <- takeMVar pl
    let (el, l') = removeElement (\(n', _, _, _) -> n == n') l 
    putMVar pl l'
    case el of {
        Just (_, _, h, _) -> putStrLn ("sending quit to " ++ n) 
                             >> hPutStrLn h "action: quit" >> hClose h;
        _                 -> return () }
    case e of
        Left err -> 
            if checkException err (ShutdownException "") then do
                let msg = case el of {
                    Just _ -> mkMessage err $ n ++ " successfully unpluged";
                    _      -> mkMessage err "Could not find plugin" }
                putStrLn $ show msg
                modifyMVar_ mq (return . (++ [msg]))
            else putStrLn $ n ++ " exception: " ++ show err
        Right _  -> putStrLn $ n ++ " exited unexpectedly"
    where
        checkException e r = show e == show r
        mkMessage x = IRCOutPrivMsg "#bots" "#bots" -- fix this

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
    return $ header:map (\(n,v,_,_) -> "    " ++ n ++ ": v" ++ v) l
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
        r <- liftIO (unplug' n plist)
        return r
        where
            unplug' :: String -> [Plugin] -> IO (Maybe String)
            unplug' n []                = return . Just $ failmsg n
            unplug' n (x@(n', _, _, tid):xs)
                | n == n'   =
                    throwTo tid (ShutdownException c) >> return Nothing
                | otherwise = unplug' n xs

            failmsg = ("Plugin " ++) . (++ " not found!")
