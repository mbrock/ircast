{-# LANGUAGE RankNTypes #-}

module Network.IRC.Pipes (
  module Network.IRC.ByteString.Parser,
  ParsingError,
  Producer',
  parseIrcMsgs,
  writingIrcMsgs
  ) where

import Data.Attoparsec.ByteString.Char8 (endOfLine)
import Data.ByteString (ByteString)

import Control.Applicative ((<*))

import Control.Monad (forever)

import Network.IRC.ByteString.Parser

import Pipes (Producer', for, await, yield, Pipe)
import Pipes.Attoparsec (parseMany, ParsingError)

parseIrcMsgs :: Monad m => Producer' ByteString m () ->
            Producer' IRCMsg m (Either ParsingError ())
parseIrcMsgs p = tossParseLength $ fmap cleanUpError p'
  where p' = parseMany (ircLine <* endOfLine) p 
        tossParseLength = flip for (yield . snd)
        cleanUpError = either (Left . fst) (const $ Right ())

writingIrcMsgs :: Monad m => Pipe IRCMsg ByteString m ()
writingIrcMsgs = forever $ await >>= yield . fromIRCMsg
