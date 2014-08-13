{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses,
             FlexibleInstances #-}

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

instance HarkerClientMonad (HonkMonadT IO) where
    clientLift = HonkMonad . lift

type HonkMonad a = HonkMonadT IO a

runHonkMonad :: HonkMonad () -> IO ()
runHonkMonad (HonkMonad s) = runHarkerClient (evalStateT s False)

main = runPlugin "honkslam" "0.1.0.0" honk runHonkMonad

honk :: HonkMonad ()
honk = do
    msg <- getMsg
    auth <- getAuth
    honk <- get
    if msg == "!honkslam" then ifauth (toggle honk)
    else if honk && map toLower msg == "honk" then honkslam msg
                                              else return ()

honkslam :: String -> HonkMonad ()
honkslam = sequence_ . take 7 . repeat . sendReply 

toggle :: Bool -> HonkMonad ()
toggle h = modify not >> sendReply ("honkslam " ++ (if h then "disabled" 
                                                         else "enabled"))
