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

import Pipes (Producer', for, yield)
import Pipes.Attoparsec (parseMany, ParsingError)

ircMsgProducer :: Monad m => Producer' ByteString m () ->
            Producer' IRCMsg m (Either ParsingError ())
ircMsgProducer p = tossParseLength $ fmap cleanUpError p'
  where p' = parseMany (ircLine <* endOfLine) p 
        tossParseLength = flip for (yield . snd)
        cleanUpError = either (Left . fst) (const $ Right ())
        