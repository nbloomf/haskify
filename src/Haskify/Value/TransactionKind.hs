{-# LANGUAGE OverloadedStrings #-}

module Haskify.Value.TransactionKind (TransactionKind(..), HasTransactionKind, theTransactionKindMaybe, theTransactionKind, setTransactionKind) where

import Data.Text (Text)
import Data.Aeson
import Haskify.HaskifyM

data TransactionKind
 = TransactionKindAuthorization
 | TransactionKindCapture
 | TransactionKindSale
 | TransactionKindVoid
 | TransactionKindRefund
 deriving Eq

class HasTransactionKind t where
  theTransactionKindMaybe :: t -> Maybe TransactionKind
  setTransactionKind :: TransactionKind -> t -> t

  theTransactionKind :: t -> HaskifyM TransactionKind
  theTransactionKind x = case theTransactionKindMaybe x of
    Nothing -> fieldDNE "TransactionKind"
    Just y -> return y

instance Show TransactionKind where
  show TransactionKindAuthorization = "authorization"
  show TransactionKindCapture = "capture"
  show TransactionKindSale = "sale"
  show TransactionKindVoid = "void"
  show TransactionKindRefund = "refund"

instance FromJSON TransactionKind where
  parseJSON = withText "TransactionKind" $ \s -> case s of
    "authorization" -> return TransactionKindAuthorization
    "capture" -> return TransactionKindCapture
    "sale" -> return TransactionKindSale
    "void" -> return TransactionKindVoid
    "refund" -> return TransactionKindRefund
    _ -> fail "expecting: authorization capture sale void refund "

instance ToJSON TransactionKind where
  toJSON TransactionKindAuthorization = String "authorization"
  toJSON TransactionKindCapture = String "capture"
  toJSON TransactionKindSale = String "sale"
  toJSON TransactionKindVoid = String "void"
  toJSON TransactionKindRefund = String "refund"

