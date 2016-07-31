{-# LANGUAGE OverloadedStrings #-}

module Haskify.Value.CarrierServiceType (CarrierServiceType(..), HasCarrierServiceType, theCarrierServiceTypeMaybe, theCarrierServiceType, setCarrierServiceType) where

import Data.Text (Text)
import Data.Aeson
import Haskify.HaskifyM

data CarrierServiceType
 = CarrierServiceTypeAPI
 | CarrierServiceTypeLegacy
 deriving Eq

class HasCarrierServiceType t where
  theCarrierServiceTypeMaybe :: t -> Maybe CarrierServiceType
  setCarrierServiceType :: CarrierServiceType -> t -> t

  theCarrierServiceType :: t -> HaskifyM CarrierServiceType
  theCarrierServiceType x = case theCarrierServiceTypeMaybe x of
    Nothing -> fieldDNE "CarrierServiceType"
    Just y -> return y

instance Show CarrierServiceType where
  show CarrierServiceTypeAPI = "api"
  show CarrierServiceTypeLegacy = "legacy"

instance FromJSON CarrierServiceType where
  parseJSON = withText "CarrierServiceType" $ \s -> case s of
    "api" -> return CarrierServiceTypeAPI
    "legacy" -> return CarrierServiceTypeLegacy
    _ -> fail "expecting: api legacy "

instance ToJSON CarrierServiceType where
  toJSON CarrierServiceTypeAPI = String "api"
  toJSON CarrierServiceTypeLegacy = String "legacy"

