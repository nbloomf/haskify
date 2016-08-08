module Main where

import Prelude hiding (readFile)

import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Lazy (readFile)
import Data.Text (Text)

import Control.Concurrent
import Control.Monad (forever)
import Control.Concurrent.Chan
import Control.Concurrent.Async

import Haskify


main :: IO ()
main = do
  args <- getArgs

  chan <- newChan
  forkIO $ writer chan

  case args of
    [cred] -> do
      (x,_) <- runHaskifyM cred (script chan)
      case x of
        Left err -> do
          putStrLn $ show err
          exitFailure
        Right response -> do
          -- sequence $ map out response
          exitSuccess

    _ -> showUsage >> exitSuccess


writer :: Chan String -> IO ()
writer chan = do
  forever $ do
    gossip <- readChan chan
    putStr gossip


script :: (Chan String) -> HaskifyM [(Text, Text, Text, IDNumber, IDNumber)]
script chan = do
  ids <- getAllProductIDs
  fmap concat $ sequence $ map (getVariantInfo chan) ids


getAllProductIDs :: HaskifyM [(IDNumber, Text)]
getAllProductIDs = do
  n <- getProductsCount
  let k = (div n 50) + (if rem n 50 > 0 then 1 else 0)
  let
    getPage t = do
      getProducts
        ( optPage t
        )
  ps <- fmap concat $ sequence $ map getPage [1..k]
  let
    getInfo p = do
      m <- theID p
      x <- theTitle p
      return (m,x)
  sequence $ map getInfo ps


-- Gets a list of all variants of a given product,
-- returning only title, sku, and ID
getVariantInfo
  :: (Chan String) -> (IDNumber, Text) -> HaskifyM [(Text, Text, Text, IDNumber, IDNumber)]
getVariantInfo chan (pid, ptitle) = do
  n <- getProductsIDVariantsCount pid
  let k = (div n 50) + (if rem n 50 > 0 then 1 else 0)
  let
    getPage t = do
      getProductsIDVariants pid
        ( optPage t
        )
  ps <- fmap concat $ sequence $ map getPage [1..k]
  let
    getInfo p = do
      x <- theTitle p
      s <- theSKU p
      m <- theID p
      liftIO $ out chan (ptitle, x, s, pid, m)
      return (ptitle, x, s, pid, m)
  sequence $ map getInfo ps


out :: Chan String -> (Text,Text,Text,IDNumber,IDNumber) -> IO ()
out chan (pname, name, sku, pnum, num) = do
  writeChan chan $
    (show pname) ++ "\t" ++ (show name) ++ "\t" ++
    (show sku) ++ "\t" ++ (show pnum) ++ "\t" ++
    (show num) ++ "\n"


showUsage :: IO ()
showUsage = do
  putStrLn "shopify-get-skus:"
  putStrLn "  get a list of all product skus from a given shopify.com shop."
  putStrLn "USAGE: shopify-get-skus CRED"
  putStrLn "  CRED is the full path of a credentials file, formatted as"
  putStrLn "    (domain)\\n(api-key)\\n(password)"
  putStrLn "OUTPUT:"
  putStrLn "  stdout: a tab-separated list consisting of product name, variant name, SKU, product ID, and variant ID."

