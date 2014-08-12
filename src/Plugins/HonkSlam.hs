{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}

module Main where

import Control.Monad.State
import Control.Monad.Trans
import Data.Char
import HarkerIRC.Client
import HarkerIRC.Types

newtype HonkMonadT m a = HonkMonad (StateT Bool (HarkerClientT m) a)
    deriving (Monad, MonadIO, Functor)

instance (Monad m) => MonadState Bool (HonkMonadT m) where
    get   = HonkMonad $ get
    put   = HonkMonad . put
    state = HonkMonad . state

instance MonadTrans HonkMonadT where
    lift = HonkMonad . lift . lift

instance (Functor m, Monad m) => HarkerClientMonad (HonkMonadT m) where
    getSocket = HonkMonad . lift $ getSocket
    getHandle = HonkMonad . lift $ getHandle
    getIRCMsg = HonkMonad . lift $ getIRCMsg
    getUser   = HonkMonad . lift $ getUser
    getNick   = HonkMonad . lift $ getNick
    getChan   = HonkMonad . lift $ getChan
    getMMsg   = HonkMonad . lift $ getMMsg
    getMsg    = HonkMonad . lift $ getMsg
    getMAuth  = HonkMonad . lift $ getMAuth
    getAuth   = HonkMonad . lift $ getAuth
    setSocket = HonkMonad . lift . setSocket
    setHandle = HonkMonad . lift . setHandle
    setIRCMsg = HonkMonad . lift . setIRCMsg

type HonkMonad a = HonkMonadT IO a

runHonkMonad :: HonkMonad () -> IO ()
runHonkMonad (HonkMonad s) = runHarkerClient (evalStateT s False)

main = runPlugin "honkslam" "0.1.0.0" honk runHonkMonad

honk :: HonkMonad ()
honk = do
    msg <- getMsg
    auth <- getAuth
    honk <- get
    if msg == "!honkslam" then
        if auth  then do 
                    modify not
                    sendReply $ "honkslam " ++ (if honk then "disabled" 
                                                        else "enabled")
                 else sendReply "you are not authenticated for that"
    else if honk && map toLower msg == "honk" then honkslam msg
                                              else return ()

honkslam :: String -> HonkMonad ()
honkslam = sequence_ . take 7 . repeat . sendReply 
