{-# LANGUAGE OverloadedStrings #-}

module Haskify.Value.InventoryManagement (InventoryManagement(..), HasInventoryManagement, theInventoryManagementMaybe, theInventoryManagement, setInventoryManagement) where

import Data.Text (Text)
import Data.Aeson
import Haskify.HaskifyM

data InventoryManagement
 = InventoryManagementBlank
 | InventoryManagementShopify
 deriving Eq

class HasInventoryManagement t where
  theInventoryManagementMaybe :: t -> Maybe InventoryManagement
  setInventoryManagement :: InventoryManagement -> t -> t

  theInventoryManagement :: t -> HaskifyM InventoryManagement
  theInventoryManagement x = case theInventoryManagementMaybe x of
    Nothing -> fieldDNE "InventoryManagement"
    Just y -> return y

instance Show InventoryManagement where
  show InventoryManagementBlank = "blank"
  show InventoryManagementShopify = "shopify"

instance FromJSON InventoryManagement where
  parseJSON = withText "InventoryManagement" $ \s -> case s of
    "blank" -> return InventoryManagementBlank
    "shopify" -> return InventoryManagementShopify
    _ -> fail "expecting: blank shopify "

instance ToJSON InventoryManagement where
  toJSON InventoryManagementBlank = String "blank"
  toJSON InventoryManagementShopify = String "shopify"

