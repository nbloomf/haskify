{-# LANGUAGE OverloadedStrings #-}

module HaskifyTest.Util where

import Data.Text
import System.Exit (exitFailure)
import Control.Monad.IO.Class (liftIO)

import Haskify

data TestStatus
 = OK | FAIL
 deriving (Eq, Show)

allOK :: [TestStatus] -> TestStatus
allOK xs = if Prelude.all (== OK) xs
  then OK
  else FAIL

runTests :: [HaskifyM TestStatus] -> HaskifyM TestStatus
runTests xs = fmap allOK (sequence xs)

testGroup :: Text -> [HaskifyM TestStatus] -> HaskifyM TestStatus
testGroup name tests = do
  logComment $ append "test group: " name
  runTests tests



{- Constant Tests -}

testOK :: HaskifyM TestStatus
testOK = do
  logComment "test \x1b[32mok\x1b[0;39;49m"
  return OK

testFAIL :: String -> HaskifyM TestStatus
testFAIL msg = do
  logComment "test \x1b[31mFAIL\x1b[0;39;49m"
  logComment $ pack msg 
  return FAIL



{- Status Tests -}

testSuccess :: Text -> HaskifyM a -> HaskifyM TestStatus
testSuccess name script = do
  logComment $ append "running " name
  x <- tryScript script
  case x of
    Right _ -> testOK
    Left _  -> testFAIL "expected success"

testFailure :: Text -> HaskifyM a -> HaskifyM TestStatus
testFailure name script = do
  logComment $ append "running " name
  x <- tryScript script
  case x of
    Left _  -> testOK
    Right _ -> testFAIL "expected failure"



{- Boolean Tests -}

testPredicate :: Text -> HaskifyM a -> (a -> Bool) -> HaskifyM TestStatus
testPredicate name script check = do
  logComment $ append "running " name
  x <- tryScript script
  case x of
    Left err -> testFAIL (show err)
    Right y  -> if check y == True
      then testOK
      else testFAIL "predicate failure"

testIsTrue :: Text -> HaskifyM Bool -> HaskifyM TestStatus
testIsTrue name script = testPredicate name script id

