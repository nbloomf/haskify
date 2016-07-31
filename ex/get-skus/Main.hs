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
    [cred,file] -> do
      (x,log) <- runHaskifyM cred (script file)
      case x of
        Left err -> do
          putStrLn $ show err
          putStrLn $ show log
          exitFailure
        Right response -> do
          -- sequence $ map out response
          exitSuccess

    _ -> showUsage >> exitSuccess


script :: FilePath -> HaskifyM [(Text, Text, Text, IDNumber, IDNumber)]
script file = do
  ids <- getAllProductIDs
  fmap concat $ sequence $ map (getVariantInfo file) ids


getAllProductIDs :: HaskifyM [(IDNumber, Text)]
getAllProductIDs = do
  n <- getProductsCount
  let k = (div n 100) + (if rem n 100 > 0 then 1 else 0)
  let
    getPage t = do
      getProducts
        ( optLimit 100
        . optPage t
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
  :: FilePath -> (IDNumber, Text) -> HaskifyM [(Text, Text, Text, IDNumber, IDNumber)]
getVariantInfo file (pid, ptitle) = do
  n <- getProductsIDVariantsCount pid
  let k = (div n 100) + (if rem n 100 > 0 then 1 else 0)
  let
    getPage t = do
      getProductsIDVariants pid
        ( optLimit 100
        . optPage t
        )
  ps <- fmap concat $ sequence $ map getPage [1..k]
  let
    getInfo p = do
      x <- theTitle p
      s <- theSKU p
      m <- theID p
      liftIO $ out file (ptitle, x, s, pid, m)
      return (ptitle, x, s, pid, m)
  sequence $ map getInfo ps


out :: FilePath -> (Text,Text,Text,IDNumber,IDNumber) -> IO ()
out file (pname, name, sku, pnum, num) = do
  appendFile file (show pname)
  appendFile file "\t"
  appendFile file (show name)
  appendFile file "\t"
  appendFile file (show sku)
  appendFile file "\t"
  appendFile file (show pnum)
  appendFile file "\t"
  appendFile file (show num)
  appendFile file "\n"


showUsage :: IO ()
showUsage = do
  putStrLn "shopify-post-product:"
  putStrLn "  post one or more products to a given shopify.com shop."
  putStrLn "USAGE: shopify-post-product CRED PROD ..."
  putStrLn "  CRED is the full path of a credentials file, formatted as"
  putStrLn "    (domain)\\n(api-key)\\n(password)"
  putStrLn "  PROD is a text file containing a JSON-formatted product"
  putStrLn "OUTPUT: a list of ID numbers and titles of the posted products."

