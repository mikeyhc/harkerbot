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

sockaddr = "/tmp/.local-echo.sock"

main = runPlugin "local-echo" "0.1.0.0" acceptFunc


echo :: Handle -> String -> IO ()
echo h m 
    | "nick: " `isPrefixOf` m = reply
    | "chan: " `isPrefixOf` m = reply
    | "msg: "  `isPrefixOf` m = reply
    | m == "-"                = reply
    | otherwise               = return ()
    where
        reply = hPutStrLn h m
