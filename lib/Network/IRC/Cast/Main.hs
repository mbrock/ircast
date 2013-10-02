{-# LANGUAGE RankNTypes, ImpredicativeTypes, OverloadedStrings #-}

module Network.IRC.Cast.Main where

import Data.Monoid
import Data.String

import Control.Concurrent (forkIO)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar

import Network.IRC.Pipes

import Network.Simple.TCP

import Pipes
import Pipes.Network.TCP (fromSocket, toSocket)

import qualified Pipes.Concurrent as PC

import Pipes.FastCGI
import Network.FastCGI (FastCGI, acceptLoop)

intro :: [IRCMsg]
intro = [
  IRCMsg {
    msgPrefix = Nothing,
    msgCmd = "NICK",
    msgParams = ["ircast"],
    msgTrail = ""
    }
  ,
  IRCMsg {
     msgPrefix = Nothing,
     msgCmd = "USER",
     msgParams = ["ircast", "0", "*"],
     msgTrail = "ircast"
     }
  ]

main :: IO ()
main = do
  sseOutput <- newTVarIO mempty
  
  let broadcast x = atomically (readTVar sseOutput >>= flip PC.send x) >> return ()

  _ <- forkIO $ do
    acceptLoop forkIO (runEffect $ handler sseOutput)
  
  withIRCPipes $ \output input -> do
    _ <- forkIO $ runEffect (each intro >-> output)
    runEffect $ for input (liftIO . broadcast)

handler :: TVar (PC.Output IRCMsg) -> Effect FastCGI ()
handler output =
  do (newOutput, newInput) <- liftIO $ PC.spawn PC.Unbounded
     liftIO $ atomically $ modifyTVar output (<> newOutput)
     PC.fromInput newInput >-> ircMsgToSse >-> streamEvents

ircMsgToSse :: Monad m => Pipe IRCMsg ServerSentEvent m r
ircMsgToSse = for cat (yield . event)
  where event x = serverSentEvent {
    eventData = Just (fromString $ show x)
  }

type IRCOutput = Consumer' IRCMsg IO ()
type IRCInput = Producer' IRCMsg IO ()

withIRCPipes :: (IRCOutput -> IRCInput -> IO ()) -> IO ()
withIRCPipes f = 
  connect "irc.freenode.net" "6667" $ \(socket, _) -> do 
    let producer = parseIrcMsgs (fromSocket socket 4096)
        consumer = writingIrcMsgs >-> toSocket socket 
     
    f consumer (fmap (const ()) producer)
