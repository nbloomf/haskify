{-# LANGUAGE OverloadedStrings #-}

module Haskify.Value.ValueType (ValueType(..), HasValueType, theValueTypeMaybe, theValueType, setValueType) where

import Data.Text (Text)
import Data.Aeson
import Haskify.HaskifyM

data ValueType
 = ValueTypeString
 | ValueTypeInteger
 deriving Eq

class HasValueType t where
  theValueTypeMaybe :: t -> Maybe ValueType
  setValueType :: ValueType -> t -> t

  theValueType :: t -> HaskifyM ValueType
  theValueType x = case theValueTypeMaybe x of
    Nothing -> fieldDNE "ValueType"
    Just y -> return y

instance Show ValueType where
  show ValueTypeString = "string"
  show ValueTypeInteger = "integer"

instance FromJSON ValueType where
  parseJSON = withText "ValueType" $ \s -> case s of
    "string" -> return ValueTypeString
    "integer" -> return ValueTypeInteger
    _ -> fail "expecting: string integer "

instance ToJSON ValueType where
  toJSON ValueTypeString = String "string"
  toJSON ValueTypeInteger = String "integer"

