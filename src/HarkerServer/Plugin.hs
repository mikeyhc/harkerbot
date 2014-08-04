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
import HarkerServer.Types
import Network
import System.Directory
import System.Exit
import System.IO
import Text.Printf

closeplugins :: MVar [Plugin] -> IO ()
closeplugins pl = takeMVar pl >>= mapM_ (\(_,_,h) -> hClose h) 
                  >> putMVar pl []

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

pluginListener :: MVar [Plugin] -> MVar Status -> IO ()
pluginListener pl status = do
    printf "Starting Plugin Thread\n"
    bracket (connectUnixPlugin pl status) (sClose . pluginSock) 
            (flip runPluginThread pluginThread)

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

pluginList :: Bot [String]
pluginList = do
    l <- gets plugins >>= liftIO . readMVar
    return $ header:map (\(n,v,_) -> "    " ++ n ++ ": v" ++ v) l
    where
        header = "registered plugins:"

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

unplug :: String -> Bot String
unplug n = do
        t     <- gets plugins
        plist <- liftIO $ readMVar t
        fst <$> liftIO (unplug' n plist)
        where
            unplug' :: String -> [Plugin] -> IO (String, [Plugin])
            unplug' n []                = return (failmsg n, [])
            unplug' n (x@(n', _, h):xs)
                | n == n'   = do
                    hPutStrLn h "action: quit"
                    hClose h
                    return (passmsg n, xs)
                | otherwise = second (x:) <$> unplug' n xs

            failmsg = (++) "Plugin " . ((++) " not found!")
            passmsg = (++) "Plugin " . ((++) " unplugged")
