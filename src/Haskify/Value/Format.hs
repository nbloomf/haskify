{-# LANGUAGE OverloadedStrings #-}

module Haskify.Value.Format (Format(..), HasFormat, theFormatMaybe, theFormat, setFormat) where

import Data.Text (Text)
import Data.Aeson
import Haskify.HaskifyM

data Format
 = FormatJSON
 | FormatXML
 deriving Eq

class HasFormat t where
  theFormatMaybe :: t -> Maybe Format
  setFormat :: Format -> t -> t

  theFormat :: t -> HaskifyM Format
  theFormat x = case theFormatMaybe x of
    Nothing -> fieldDNE "Format"
    Just y -> return y

instance Show Format where
  show FormatJSON = "json"
  show FormatXML = "xml"

instance FromJSON Format where
  parseJSON = withText "Format" $ \s -> case s of
    "json" -> return FormatJSON
    "xml" -> return FormatXML
    _ -> fail "expecting: json xml "

instance ToJSON Format where
  toJSON FormatJSON = String "json"
  toJSON FormatXML = String "xml"

