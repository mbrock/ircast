{-# LANGUAGE RankNTypes #-}

module Network.IRC.Pipes (
  module Network.IRC.ByteString.Parser,
  ParsingError,
  Producer',
  ircMsgProducer
  ) where

import Data.Attoparsec.ByteString.Char8 (endOfLine)
import Data.ByteString (ByteString)

import Control.Applicative ((<*))

import Network.IRC.ByteString.Parser

import Pipes (Producer, Producer', for, yield)
import Pipes.Attoparsec (parseMany, ParsingError)

type Result m = Either (ParsingError, Producer ByteString m ()) ()

ircMsgProducer :: Monad m => Producer' ByteString m () ->
            Producer' IRCMsg m (Result m)
ircMsgProducer p = tossParseLength $ parseMany (ircLine <* endOfLine) p
  where tossParseLength = flip for (yield . snd)
