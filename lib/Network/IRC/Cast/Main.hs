{-# LANGUAGE RankNTypes, ImpredicativeTypes, OverloadedStrings #-}

module Network.IRC.Cast.Main where

import Data.ByteString (ByteString)
import qualified Data.Text.Encoding as E

import Control.Concurrent (forkIO)

import Network.IRC.Pipes

import Network.Simple.TCP

import Pipes
import Pipes.Network.TCP

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
  withIRCPipes $ \output input -> do
    _ <- forkIO $ runEffect (each intro >-> output)
    runEffect (for input (lift . print))

type IRCOutput = Consumer' IRCMsg IO ()
type IRCInput = Producer' IRCMsg IO ()

withIRCPipes :: (IRCOutput -> IRCInput -> IO ()) -> IO ()
withIRCPipes f = 
  connect "irc.freenode.net" "6667" $ \(socket, _) -> do 
    let producer = parseIrcMsgs (fromSocket socket 4096)
        consumer = writingIrcMsgs >-> toSocket socket 
     
    f consumer (fmap (const ()) producer)
