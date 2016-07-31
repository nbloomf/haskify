module Main where

import System.Exit (exitSuccess, exitFailure)
import System.Environment (getArgs)

import Haskify
import HaskifyTest
import HaskifyTest.Util


main :: IO ()
main = do
  args <- getArgs

  path <- case args of
            [x] -> return x
            _   -> showUsage >> exitFailure

  (result,_) <- runHaskifyM path allTests

  case result of
    Right OK -> do
      putStrLn $ "all tests complete"
      exitSuccess
    Right FAIL -> do
      putStrLn $ "there were failing tests!"
      exitFailure
    Left err -> do
      putStrLn $ "script error"
      putStrLn $ show err
      exitFailure



showUsage :: IO ()
showUsage = do
  putStrLn "usage: haskify-test CRED"
  putStrLn "  CRED: full path to credentials of test shop"
  putStrLn "        DOMAIN\\nAPIKEY\\nPASSWORD"
