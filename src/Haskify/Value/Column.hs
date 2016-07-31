{-# LANGUAGE OverloadedStrings #-}

module Haskify.Value.Column (Column(..), HasColumn, theColumnMaybe, theColumn, setColumn) where

import Data.Text (Text)
import Data.Aeson
import Haskify.HaskifyM

data Column
 = ColumnTitle
 | ColumnType
 | ColumnVendor
 | ColumnVariantPrice
 | ColumnVariantCompareAtPrice
 | ColumnVariantWeight
 | ColumnVariantInventory
 | ColumnVariantTitle
 deriving Eq

class HasColumn t where
  theColumnMaybe :: t -> Maybe Column
  setColumn :: Column -> t -> t

  theColumn :: t -> HaskifyM Column
  theColumn x = case theColumnMaybe x of
    Nothing -> fieldDNE "Column"
    Just y -> return y

instance Show Column where
  show ColumnTitle = "title"
  show ColumnType = "type"
  show ColumnVendor = "vendor"
  show ColumnVariantPrice = "variant_price"
  show ColumnVariantCompareAtPrice = "variant_compare_at_price"
  show ColumnVariantWeight = "variant_weight"
  show ColumnVariantInventory = "variant_inventory"
  show ColumnVariantTitle = "variant_title"

instance FromJSON Column where
  parseJSON = withText "Column" $ \s -> case s of
    "title" -> return ColumnTitle
    "type" -> return ColumnType
    "vendor" -> return ColumnVendor
    "variant_price" -> return ColumnVariantPrice
    "variant_compare_at_price" -> return ColumnVariantCompareAtPrice
    "variant_weight" -> return ColumnVariantWeight
    "variant_inventory" -> return ColumnVariantInventory
    "variant_title" -> return ColumnVariantTitle
    _ -> fail "expecting: title type vendor variant_price variant_compare_at_price variant_weight variant_inventory variant_title "

instance ToJSON Column where
  toJSON ColumnTitle = String "title"
  toJSON ColumnType = String "type"
  toJSON ColumnVendor = String "vendor"
  toJSON ColumnVariantPrice = String "variant_price"
  toJSON ColumnVariantCompareAtPrice = String "variant_compare_at_price"
  toJSON ColumnVariantWeight = String "variant_weight"
  toJSON ColumnVariantInventory = String "variant_inventory"
  toJSON ColumnVariantTitle = String "variant_title"

