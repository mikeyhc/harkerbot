{-# LANGUAGE DeriveDataTypeable #-}

module HarkerServer.Types where

import Control.Concurrent (forkFinally, MVar, newEmptyMVar, putMVar, takeMVar
                          ,yield, ThreadId, tryTakeMVar, readMVar, newMVar
                          ,threadDelay, modifyMVar_, forkIO)
import Control.Exception.Base
import Control.Monad.Reader
import Control.Monad.State
import Data.Typeable
import HarkerIRC.Types
import Network
import System.IO
import System.Time

data BotCore  = BotCore { socket    :: Handle 
                        , starttime :: ClockTime
                        , plugintid :: ThreadId
                        , nick      :: String
                        , pass      :: String
                        , chan      :: String
                        , maxlines  :: Int
                        }
data BotBrain = BotBrain { botauth      :: Maybe User
                         , honkslam     :: Bool
                         , pingalert    :: Bool
                         , plugins      :: MVar [Plugin]
                         , childstatus  :: MVar Status
                         , messagequeue :: MVar OutMessageQueue
                         }
data PluginCore = PluginCore { pluginVar      :: MVar [Plugin]
                             , statusVar      :: MVar Status
                             , pluginMsgQueue :: MVar OutMessageQueue
                             , pluginSock     :: Socket
                             }

data Status = Starting | Running | Dead
data TimeOutException = TimeOutException
    deriving (Show, Typeable)
instance Exception TimeOutException
data ShutdownException = ShutdownException 
    { shutdownChannel :: String
    } deriving (Typeable)

instance Exception ShutdownException

instance Show ShutdownException where
    show (ShutdownException _) = "ShutdownException"

isRunning Running  = True
isRunning Starting = True
isRunning _        = False

--                     name,   version, socket, tid
type Plugin         = (String, String, Handle, ThreadId)
type Bot a          = ReaderT BotCore (StateT BotBrain IO) a
type PluginThread a = ReaderT PluginCore IO a

runbot :: BotBrain -> BotCore -> Bot a -> IO ()
runbot bb bc f = runStateT (runReaderT f bc) bb >> return ()

runPluginThread :: PluginCore -> PluginThread a -> IO ()
runPluginThread pc f = runReaderT f pc >> return ()

emptyBrain :: IO BotBrain
emptyBrain = do
    v <- newMVar []
    d <- newEmptyMVar
    c <- newMVar []
    return $ BotBrain Nothing False False v d c
