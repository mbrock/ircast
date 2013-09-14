{-# LANGUAGE RankNTypes, OverloadedStrings, GADTs #-}

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
  [":wolfe.freenode.net NOTICE * :*** Looking up your ",
   "hostname...\r\n",
   ":wolfe.freenode.net NOTICE * :*** Checking Ident\r\n",
   ":wolfe.freenode.net NOTICE * :*** Found your hostname\r\n"]
  
freenodeWelcomeMsgs =
  [IRCMsg {msgPrefix = Just (Right "wolfe.freenode.net"), 
           msgCmd = "NOTICE", msgParams = ["*"], 
           msgTrail = "*** Looking up your hostname..."},
   IRCMsg {msgPrefix = Just (Right "wolfe.freenode.net"), 
           msgCmd = "NOTICE", msgParams = ["*"], 
           msgTrail = "*** Checking Ident"},
   IRCMsg {msgPrefix = Just (Right "wolfe.freenode.net"), 
           msgCmd = "NOTICE", msgParams = ["*"], 
           msgTrail = "*** Found your hostname"} ]

parseTests = testGroup "IRC pipe tests"
  [ testCase "Reading Freenode welcome" $
      parseAll freenodeWelcome @?= Just freenodeWelcomeMsgs
  , testCase "Writing Freenode welcome" $
      renderAll freenodeWelcomeMsgs @?= mconcat freenodeWelcome
  ]
    
-- Use the IRC parsing pipe to parse a sequence of ByteStrings.
parseAll :: [ByteString] -> Maybe [IRCMsg]
parseAll xs = eitherToMaybe $ produce (parseIrcMsgs (each xs))

renderAll :: [IRCMsg] -> ByteString
renderAll xs = execWriter (gather (each xs >-> writingIrcMsgs))

produce :: Error e => Producer' a (Writer [a]) (Either e b) -> Either e [a]
produce p = execWriterError (collect (errorP p))

collect :: (MonadWriter w m, w ~ [a]) => Producer' a m r -> m r
collect p = runEffect (for p (tell . (:[])))

gather :: (MonadWriter w m) => Producer' w m r -> m r
gather p = runEffect (for p tell)

execWriterError :: Error e => ErrorT e (Writer w) a -> Either e w
execWriterError m = 
  case runWriter (runErrorT m) of
    (Left e, _)  -> Left e
    (Right _, a) -> Right a

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe = either (const Nothing) Just


