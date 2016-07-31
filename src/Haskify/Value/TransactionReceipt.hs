{-# LANGUAGE OverloadedStrings #-}

module Haskify.Value.TransactionReceipt (TransactionReceipt(..), HasTransactionReceipt, theTransactionReceiptMaybe, theTransactionReceipt, setTransactionReceipt) where

import Data.Text (Text)
import Data.Aeson
import Haskify.HaskifyM

data TransactionReceipt
 = TransactionReceiptTestCase
 | TransactionReceiptAuthorization
 deriving Eq

class HasTransactionReceipt t where
  theTransactionReceiptMaybe :: t -> Maybe TransactionReceipt
  setTransactionReceipt :: TransactionReceipt -> t -> t

  theTransactionReceipt :: t -> HaskifyM TransactionReceipt
  theTransactionReceipt x = case theTransactionReceiptMaybe x of
    Nothing -> fieldDNE "TransactionReceipt"
    Just y -> return y

instance Show TransactionReceipt where
  show TransactionReceiptTestCase = "testcase"
  show TransactionReceiptAuthorization = "authorization"

instance FromJSON TransactionReceipt where
  parseJSON = withText "TransactionReceipt" $ \s -> case s of
    "testcase" -> return TransactionReceiptTestCase
    "authorization" -> return TransactionReceiptAuthorization
    _ -> fail "expecting: testcase authorization "

instance ToJSON TransactionReceipt where
  toJSON TransactionReceiptTestCase = String "testcase"
  toJSON TransactionReceiptAuthorization = String "authorization"

