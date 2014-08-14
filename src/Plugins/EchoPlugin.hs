{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses,
             FlexibleInstances #-}

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

instance HarkerClientMonad (EchoMonadT IO) where
    clientLift = EchoMonad . lift 

type EchoMonad a = EchoMonadT IO a

runEchoMonad :: EchoMonad () -> IO ()
runEchoMonad (EchoMonad s) = runHarkerClient (evalStateT s False)

main = runPlugin "echo" "0.1.0.0" echo runEchoMonad

echo :: EchoMonad ()
echo = do
    msg <- getMsg
    echo <- get
    liftIO $ putStrLn ("got msg: " ++ msg)
    if msg == "!echo" then ifauth (toggle echo)
    else if msg == "!help" then sendReply "!echo: enable/disable echoing"
    else if echo && head msg /= '!' then sendReply msg
                                    else return ()

toggle :: Bool -> EchoMonad ()
toggle e = modify not >> sendReply ("echo " ++ (if e then "disabled" 
                                                     else "enabled"))
