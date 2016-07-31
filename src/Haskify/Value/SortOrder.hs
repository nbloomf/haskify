{-# LANGUAGE OverloadedStrings #-}

module Haskify.Value.SortOrder (SortOrder(..), HasSortOrder, theSortOrderMaybe, theSortOrder, setSortOrder) where

import Data.Text (Text)
import Data.Aeson
import Haskify.HaskifyM

data SortOrder
 = SortOrderAlphaAsc
 | SortOrderAlphaDesc
 | SortOrderBestSelling
 | SortOrderCreated
 | SortOrderCreatedDesc
 | SortOrderManual
 | SortOrderPriceAsc
 | SortOrderPriceDesc
 deriving Eq

class HasSortOrder t where
  theSortOrderMaybe :: t -> Maybe SortOrder
  setSortOrder :: SortOrder -> t -> t

  theSortOrder :: t -> HaskifyM SortOrder
  theSortOrder x = case theSortOrderMaybe x of
    Nothing -> fieldDNE "SortOrder"
    Just y -> return y

instance Show SortOrder where
  show SortOrderAlphaAsc = "alpha-asc"
  show SortOrderAlphaDesc = "alpha-desc"
  show SortOrderBestSelling = "best-selling"
  show SortOrderCreated = "created"
  show SortOrderCreatedDesc = "created-desc"
  show SortOrderManual = "manual"
  show SortOrderPriceAsc = "price-asc"
  show SortOrderPriceDesc = "price-desc"

instance FromJSON SortOrder where
  parseJSON = withText "SortOrder" $ \s -> case s of
    "alpha-asc" -> return SortOrderAlphaAsc
    "alpha-desc" -> return SortOrderAlphaDesc
    "best-selling" -> return SortOrderBestSelling
    "created" -> return SortOrderCreated
    "created-desc" -> return SortOrderCreatedDesc
    "manual" -> return SortOrderManual
    "price-asc" -> return SortOrderPriceAsc
    "price-desc" -> return SortOrderPriceDesc
    _ -> fail "expecting: alpha-asc alpha-desc best-selling created created-desc manual price-asc price-desc "

instance ToJSON SortOrder where
  toJSON SortOrderAlphaAsc = String "alpha-asc"
  toJSON SortOrderAlphaDesc = String "alpha-desc"
  toJSON SortOrderBestSelling = String "best-selling"
  toJSON SortOrderCreated = String "created"
  toJSON SortOrderCreatedDesc = String "created-desc"
  toJSON SortOrderManual = String "manual"
  toJSON SortOrderPriceAsc = String "price-asc"
  toJSON SortOrderPriceDesc = String "price-desc"

