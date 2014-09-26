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
                         , pingalert    :: Bool
                         , plugins      :: MVar [Plugin]
                         , childstatus  :: MVar Status
                         , messagequeue :: MVar OutMessageQueue
                         , ignorelist   :: [Nick]
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
    { shutdownChannel :: Maybe String
    } deriving (Typeable)

instance Exception ShutdownException

instance Show ShutdownException where
    show (ShutdownException _) = "ShutdownException"

isRunning Running  = True
isRunning Starting = True
isRunning _        = False

--                     name,   version, socket, tid
data Plugin = Plugin
            { pluginName      :: String
            , pluginVersion   :: String
            , pluginHandle    :: Handle
            , pluginThreadId  :: ThreadId
            }

type Bot a          = ReaderT BotCore (StateT BotBrain IO) a
type PluginThread a = ReaderT PluginCore IO a

runbot :: BotBrain -> BotCore -> Bot a -> IO ()
runbot bb bc f = void $ runStateT (runReaderT f bc) bb 

runPluginThread :: PluginCore -> PluginThread a -> IO ()
runPluginThread pc f = void $ runReaderT f pc 

emptyBrain :: IO BotBrain
emptyBrain = do
    v <- newMVar []
    d <- newEmptyMVar
    c <- newMVar []
    return $ BotBrain Nothing False v d c []
