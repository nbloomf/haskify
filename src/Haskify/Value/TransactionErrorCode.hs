{-# LANGUAGE OverloadedStrings #-}

module Haskify.Value.TransactionErrorCode (TransactionErrorCode(..), HasTransactionErrorCode, theTransactionErrorCodeMaybe, theTransactionErrorCode, setTransactionErrorCode) where

import Data.Text (Text)
import Data.Aeson
import Haskify.HaskifyM

data TransactionErrorCode
 = TransactionErrorCodeIncorrectNumber
 | TransactionErrorCodeInvalidNumber
 | TransactionErrorCodeInvalidExpiryDate
 | TransactionErrorCodeInvalidCVC
 | TransactionErrorCodeExpiredCard
 | TransactionErrorCodeIncorrectCVC
 | TransactionErrorCodeIncorrectZip
 | TransactionErrorCodeIncorrectAddress
 | TransactionErrorCodeCardDeclined
 | TransactionErrorCodeProcessingError
 | TransactionErrorCodeCallIssuer
 | TransactionErrorCodePickUpCard
 deriving Eq

class HasTransactionErrorCode t where
  theTransactionErrorCodeMaybe :: t -> Maybe TransactionErrorCode
  setTransactionErrorCode :: TransactionErrorCode -> t -> t

  theTransactionErrorCode :: t -> HaskifyM TransactionErrorCode
  theTransactionErrorCode x = case theTransactionErrorCodeMaybe x of
    Nothing -> fieldDNE "TransactionErrorCode"
    Just y -> return y

instance Show TransactionErrorCode where
  show TransactionErrorCodeIncorrectNumber = "incorrect_number"
  show TransactionErrorCodeInvalidNumber = "invalid_number"
  show TransactionErrorCodeInvalidExpiryDate = "invalid_expiry_date"
  show TransactionErrorCodeInvalidCVC = "invalid_cvc"
  show TransactionErrorCodeExpiredCard = "expired_card"
  show TransactionErrorCodeIncorrectCVC = "incorrect_cvc"
  show TransactionErrorCodeIncorrectZip = "incorrect_zip"
  show TransactionErrorCodeIncorrectAddress = "incorrect_address"
  show TransactionErrorCodeCardDeclined = "card_declined"
  show TransactionErrorCodeProcessingError = "processing_error"
  show TransactionErrorCodeCallIssuer = "call_issuer"
  show TransactionErrorCodePickUpCard = "pick_up_card"

instance FromJSON TransactionErrorCode where
  parseJSON = withText "TransactionErrorCode" $ \s -> case s of
    "incorrect_number" -> return TransactionErrorCodeIncorrectNumber
    "invalid_number" -> return TransactionErrorCodeInvalidNumber
    "invalid_expiry_date" -> return TransactionErrorCodeInvalidExpiryDate
    "invalid_cvc" -> return TransactionErrorCodeInvalidCVC
    "expired_card" -> return TransactionErrorCodeExpiredCard
    "incorrect_cvc" -> return TransactionErrorCodeIncorrectCVC
    "incorrect_zip" -> return TransactionErrorCodeIncorrectZip
    "incorrect_address" -> return TransactionErrorCodeIncorrectAddress
    "card_declined" -> return TransactionErrorCodeCardDeclined
    "processing_error" -> return TransactionErrorCodeProcessingError
    "call_issuer" -> return TransactionErrorCodeCallIssuer
    "pick_up_card" -> return TransactionErrorCodePickUpCard
    _ -> fail "expecting: incorrect_number invalid_number invalid_expiry_date invalid_cvc expired_card incorrect_cvc incorrect_zip incorrect_address card_declined processing_error call_issuer pick_up_card "

instance ToJSON TransactionErrorCode where
  toJSON TransactionErrorCodeIncorrectNumber = String "incorrect_number"
  toJSON TransactionErrorCodeInvalidNumber = String "invalid_number"
  toJSON TransactionErrorCodeInvalidExpiryDate = String "invalid_expiry_date"
  toJSON TransactionErrorCodeInvalidCVC = String "invalid_cvc"
  toJSON TransactionErrorCodeExpiredCard = String "expired_card"
  toJSON TransactionErrorCodeIncorrectCVC = String "incorrect_cvc"
  toJSON TransactionErrorCodeIncorrectZip = String "incorrect_zip"
  toJSON TransactionErrorCodeIncorrectAddress = String "incorrect_address"
  toJSON TransactionErrorCodeCardDeclined = String "card_declined"
  toJSON TransactionErrorCodeProcessingError = String "processing_error"
  toJSON TransactionErrorCodeCallIssuer = String "call_issuer"
  toJSON TransactionErrorCodePickUpCard = String "pick_up_card"

