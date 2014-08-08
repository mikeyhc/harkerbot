{-# LANGUAGE DeriveDataTypeable #-}

import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.List (isPrefixOf)
import Data.Typeable
import Network
import System.Directory
import System.Exit
import System.IO

data QuitException = QuitException String
    deriving (Typeable, Show)
instance Exception QuitException

sockaddr = "/tmp/.local-echo.sock"

main = do
    s <- listenOn (UnixSocket sockaddr)
    h <- connectTo "localhost" (UnixSocket "/tmp/.harker-server.sock")
    hPutStrLn h "name: local-echo"
    hPutStrLn h "version: 0.1.0.0"
    hPutStrLn h $ "server: " ++ sockaddr
    r <- hGetLine h
    if r == "registered" 
        then do
            tid <- myThreadId
            loopfunc (acceptfunc s tid) `catch` quitcatch
        else hPutStrLn stderr $ "error: " ++ r
    b <- doesFileExist sockaddr 
    when b $ removeFile sockaddr


quitcatch :: QuitException -> IO ()
quitcatch _ = do 
    putStrLn "quiting"
    return ()

trim [x] 
    | x == '\n' = []
    | otherwise = [x]
trim (x:xs) = x:trim xs

loopfunc a = a >> loopfunc a

acceptfunc :: Socket -> ThreadId -> IO ()
acceptfunc s tid = accept s >>= \(h, _, _) ->
    void $ forkFinally (forkfunc h tid) (exitfunc h)

exitfunc h _ = hClose h

forkfunc :: Handle -> ThreadId -> IO ()
forkfunc h tid = do
    l <- trim <$> hGetLine h
    if l == "action: quit" 
        then exitfunc h (Right ()) >> throwTo tid (QuitException "done")
        else do
            putStrLn l
            echo h l
            forkfunc h tid

echo :: Handle -> String -> IO ()
echo h m 
    | "nick: " `isPrefixOf` m = reply
    | "chan: " `isPrefixOf` m = reply
    | "msg: "  `isPrefixOf` m = reply
    | m == "-"                = reply
    | otherwise               = return ()
    where
        reply = hPutStrLn h m
