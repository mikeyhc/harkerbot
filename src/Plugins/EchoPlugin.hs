{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}

module Main where

import Control.Monad.State
import Control.Monad.Trans
import HarkerIRC.Client
import HarkerIRC.Types

newtype EchoMonadT m a = EchoMonad (StateT Bool (HarkerClientT m) a)
    deriving (Monad, MonadIO, Functor)

instance (Monad m) => MonadState Bool (EchoMonadT m) where
    get   = EchoMonad $ get
    put   = EchoMonad . put
    state = EchoMonad . state

instance MonadTrans EchoMonadT where
    lift = EchoMonad . lift . lift

instance (Functor m, Monad m) => HarkerClientMonad (EchoMonadT m) where
    getSocket = EchoMonad . lift $ getSocket
    getHandle = EchoMonad . lift $ getHandle
    getIRCMsg = EchoMonad . lift $ getIRCMsg
    getUser   = EchoMonad . lift $ getUser
    getNick   = EchoMonad . lift $ getNick
    getChan   = EchoMonad . lift $ getChan
    getMMsg   = EchoMonad . lift $ getMMsg
    getMsg    = EchoMonad . lift $ getMsg
    getMAuth  = EchoMonad . lift $ getMAuth
    getAuth   = EchoMonad . lift $ getAuth
    setSocket = EchoMonad . lift . setSocket
    setHandle = EchoMonad . lift . setHandle
    setIRCMsg = EchoMonad . lift . setIRCMsg

type EchoMonad a = EchoMonadT IO a

runEchoMonad :: EchoMonad () -> IO ()
runEchoMonad (EchoMonad s) = runHarkerClient (evalStateT s False)

main = runPlugin "echo" "0.1.0.0" echo runEchoMonad

echo :: EchoMonad ()
echo = do
    liftIO $ putStrLn "main echo"
    msg <- getMsg
    auth <- getAuth
    echo <- get
    liftIO $ putStrLn ("got msg: " ++ msg)
    if msg == "!echo" then
        if auth  then do 
                    modify not
                    sendReply $ "echo " ++ (if echo then "disabled" 
                                                    else "enabled")
                 else sendReply "you are not authenticated for that"
    else if echo then sendReply msg
                 else return ()
