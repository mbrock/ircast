{-# LANGUAGE RankNTypes, OverloadedStrings #-}

import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.HUnit

import Data.ByteString (ByteString)
import Data.Either

import qualified Data.Text.Encoding as E

import Control.Monad.Writer
import Control.Monad.Error

import Network.IRC.Pipes

import Pipes
import Pipes.Lift

main = defaultMain tests

tests = testGroup "Tests" [parseTests]

freenodeWelcome = map E.encodeUtf8
  [":wolfe.freenode.net NOTICE * :*** Looking up your", 
   " hostname...\r\n",
   ":wolfe.freenode.net NOTICE * :*** Checking Ident\r\n",
   ":wolfe.freenode.net NOTICE * :*** Found your hostname\r\n"]
  
parseTests = testGroup "IRC parsing tests"
  [ testCase "Freenode welcome" $
      parseAll freenodeWelcome @?= 
        Just [IRCMsg {msgPrefix = Just (Right "wolfe.freenode.net"), 
                      msgCmd = "NOTICE", msgParams = ["*"], 
                      msgTrail = "*** Looking up your hostname..."},
              IRCMsg {msgPrefix = Just (Right "wolfe.freenode.net"), 
                      msgCmd = "NOTICE", msgParams = ["*"], 
                      msgTrail = "*** Checking Ident"},
              IRCMsg {msgPrefix = Just (Right "wolfe.freenode.net"), 
                      msgCmd = "NOTICE", msgParams = ["*"], 
                      msgTrail = "*** Found your hostname"} ] ]
    
produce :: Error e => Producer' a (Writer [a]) (Either e b) -> Either e [a]
produce p =
  case runWriter $ runErrorT $ runEffect (for (errorP p) (tell . (:[]))) of
    (Left e, _) -> Left e
    (Right _, xs) -> Right xs

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe = either (const Nothing) Just

-- Use the IRC parsing pipe to parse a sequence of ByteStrings.
parseAll :: [ByteString] -> Maybe [IRCMsg]
parseAll xs = eitherToMaybe $ produce (ircMsgProducer (each xs))

