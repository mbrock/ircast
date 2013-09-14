{-# LANGUAGE RankNTypes, OverloadedStrings #-}

module Network.IRC.Cast.Main where

import Data.ByteString (ByteString)

import qualified Network.IRC.Pipes as IRCPipe

import qualified Data.Text.Encoding as E

import Network.Simple.TCP

import Pipes
--import Pipes.Concurrent
import Pipes.Network.TCP

testMessages :: [ByteString]
testMessages = map E.encodeUtf8
  [":wolfe.freenode.net NOTICE * :*** Looking up your", 
   " hostname...\r\n",
   ":wolfe.freenode.net NOTICE * :*** Checking Ident\r\n",
   ":wolfe.freenode.net NOTICE * :*** Found your hostname\r\n"]

main :: IO ()
main = withReadPipe go where 
  
  go :: Producer' ByteString IO () -> IO ()
  go readPipe = runEffect (printMsgs readPipe) >>= handleResult
  
  handleResult :: Either IRCPipe.ParsingError () -> IO ()
  handleResult = either print (const $ return ()) 
     
  printMsgs :: Producer' ByteString IO () -> 
               Effect IO (Either IRCPipe.ParsingError ())
  printMsgs p = for (IRCPipe.ircMsgProducer p) (lift . print)
       
withReadPipe :: (Producer' ByteString IO () -> IO ()) -> IO ()
withReadPipe f = f (each testMessages)

withIRCReadPipe :: (Producer' ByteString IO () -> IO ()) -> IO ()
withIRCReadPipe f = connect "irc.freenode.net" "6667" $
                      \(socket, _) -> f (fromSocket socket 4096)
