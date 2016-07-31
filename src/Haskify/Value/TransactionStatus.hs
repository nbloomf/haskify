{-# LANGUAGE OverloadedStrings #-}

module Haskify.Value.TransactionStatus (TransactionStatus(..), HasTransactionStatus, theTransactionStatusMaybe, theTransactionStatus, setTransactionStatus) where

import Data.Text (Text)
import Data.Aeson
import Haskify.HaskifyM

data TransactionStatus
 = TransactionStatusPending
 | TransactionStatusFailure
 | TransactionStatusSuccess
 | TransactionStatusError
 deriving Eq

class HasTransactionStatus t where
  theTransactionStatusMaybe :: t -> Maybe TransactionStatus
  setTransactionStatus :: TransactionStatus -> t -> t

  theTransactionStatus :: t -> HaskifyM TransactionStatus
  theTransactionStatus x = case theTransactionStatusMaybe x of
    Nothing -> fieldDNE "TransactionStatus"
    Just y -> return y

instance Show TransactionStatus where
  show TransactionStatusPending = "pending"
  show TransactionStatusFailure = "failure"
  show TransactionStatusSuccess = "success"
  show TransactionStatusError = "error"

instance FromJSON TransactionStatus where
  parseJSON = withText "TransactionStatus" $ \s -> case s of
    "pending" -> return TransactionStatusPending
    "failure" -> return TransactionStatusFailure
    "success" -> return TransactionStatusSuccess
    "error" -> return TransactionStatusError
    _ -> fail "expecting: pending failure success error "

instance ToJSON TransactionStatus where
  toJSON TransactionStatusPending = String "pending"
  toJSON TransactionStatusFailure = String "failure"
  toJSON TransactionStatusSuccess = String "success"
  toJSON TransactionStatusError = String "error"

