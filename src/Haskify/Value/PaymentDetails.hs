{-# LANGUAGE OverloadedStrings #-}

module Haskify.Value.PaymentDetails (PaymentDetails(..), HasPaymentDetails, thePaymentDetailsMaybe, thePaymentDetails, setPaymentDetails) where

import Data.Text (Text)
import Data.Aeson
import Haskify.HaskifyM

data PaymentDetails
 = PaymentDetailsAVSResultCode
 | PaymentDetailsCreditCardBIN
 | PaymentDetailsCreditCardCompany
 | PaymentDetailsCreditCardNumber
 | PaymentDetailsCCVResultCode
 deriving Eq

class HasPaymentDetails t where
  thePaymentDetailsMaybe :: t -> Maybe PaymentDetails
  setPaymentDetails :: PaymentDetails -> t -> t

  thePaymentDetails :: t -> HaskifyM PaymentDetails
  thePaymentDetails x = case thePaymentDetailsMaybe x of
    Nothing -> fieldDNE "PaymentDetails"
    Just y -> return y

instance Show PaymentDetails where
  show PaymentDetailsAVSResultCode = "avs_result_code"
  show PaymentDetailsCreditCardBIN = "credit_card_bin"
  show PaymentDetailsCreditCardCompany = "credit_card_company"
  show PaymentDetailsCreditCardNumber = "credit_card_number"
  show PaymentDetailsCCVResultCode = "ccv_result_code"

instance FromJSON PaymentDetails where
  parseJSON = withText "PaymentDetails" $ \s -> case s of
    "avs_result_code" -> return PaymentDetailsAVSResultCode
    "credit_card_bin" -> return PaymentDetailsCreditCardBIN
    "credit_card_company" -> return PaymentDetailsCreditCardCompany
    "credit_card_number" -> return PaymentDetailsCreditCardNumber
    "ccv_result_code" -> return PaymentDetailsCCVResultCode
    _ -> fail "expecting: avs_result_code credit_card_bin credit_card_company credit_card_number ccv_result_code "

instance ToJSON PaymentDetails where
  toJSON PaymentDetailsAVSResultCode = String "avs_result_code"
  toJSON PaymentDetailsCreditCardBIN = String "credit_card_bin"
  toJSON PaymentDetailsCreditCardCompany = String "credit_card_company"
  toJSON PaymentDetailsCreditCardNumber = String "credit_card_number"
  toJSON PaymentDetailsCCVResultCode = String "ccv_result_code"

