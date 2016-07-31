module Main where

import Prelude hiding (readFile)

import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Lazy (readFile)
import Data.Text (Text)

import Haskify


main :: IO ()
main = do
  args <- getArgs

  case args of
    (cred:paths) -> do
      (x,log) <- runHaskifyM cred (script paths)
      case x of
        Left err -> do
          putStrLn $ show err
          putStrLn $ show log
          exitFailure
        Right response -> do
          sequence $ map out response
          exitSuccess

    _ -> showUsage >> exitSuccess



-- Takes a list of paths, parses each as a JSON-encoded
-- product, posts, and returns a list of resulting
-- ID number - title pairs (for later use)
script :: [FilePath] -> HaskifyM [(IDNumber, Text)]
script paths = sequence $ map script' paths
  where
    script' :: FilePath -> HaskifyM (IDNumber, Text)
    script' p = do
      x <- (liftIO $ readFile p) >>= tryDecode
      prod <- postProducts (const x)
      n <- theID prod
      t <- theTitle prod
      return (n,t)


out :: (IDNumber, Text) -> IO ()
out (n,t) = do
  putStr (show n)
  putStr ":"
  putStrLn (show t)


showUsage :: IO ()
showUsage = do
  putStrLn "shopify-post-product:"
  putStrLn "  post one or more products to a given shopify.com shop."
  putStrLn "USAGE: shopify-post-product CRED PROD ..."
  putStrLn "  CRED is the full path of a credentials file, formatted as"
  putStrLn "    (domain)\\n(api-key)\\n(password)"
  putStrLn "  PROD is a text file containing a JSON-formatted product"
  putStrLn "OUTPUT: a list of ID numbers and titles of the posted products."

