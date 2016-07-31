{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Haskify.Data.CarrierServiceItem (
  CarrierServiceItem()
  , HasCarrierServiceItem, theCarrierServiceItemMaybe, setCarrierServiceItem, theCarrierServiceItem, HasCarrierServiceItems, theCarrierServiceItemsMaybe, setCarrierServiceItems, theCarrierServiceItems
) where

import Prelude hiding (id, zip)

import Data.Text (Text, pack, concat, intercalate)
import Data.Aeson
import Data.Time.Clock (UTCTime)

import Haskify.Types
import Haskify.DataFields
import Haskify.JSON
import Haskify.HaskifyM

data CarrierServiceItem = CarrierServiceItem
 { name                :: Maybe Text
 , sku                 :: Maybe Text
 , quantity            :: Maybe Int
 , grams               :: Maybe Int
 , price               :: Maybe Int
 , vendor              :: Maybe Text
 , requires_shipping   :: Maybe Bool
 , taxable             :: Maybe Bool
 , fulfillment_service :: Maybe Text
 } deriving (Eq, Show)

class HasCarrierServiceItem t where
  theCarrierServiceItemMaybe :: t -> Maybe CarrierServiceItem
  setCarrierServiceItem :: CarrierServiceItem -> t -> t

  theCarrierServiceItem :: t -> HaskifyM CarrierServiceItem
  theCarrierServiceItem x = case theCarrierServiceItemMaybe x of
    Nothing -> fieldDNE "CarrierServiceItem"
    Just y -> return y

class HasCarrierServiceItems t where
  theCarrierServiceItemsMaybe :: t -> Maybe [CarrierServiceItem]
  setCarrierServiceItems :: [CarrierServiceItem] -> t -> t

  theCarrierServiceItems :: t -> HaskifyM [CarrierServiceItem]
  theCarrierServiceItems x = case theCarrierServiceItemsMaybe x of
    Nothing -> fieldDNE "CarrierServiceItem"
    Just y -> return y

instance NullObject CarrierServiceItem where
  nullObject = CarrierServiceItem
    { name                = Nothing
    , sku                 = Nothing
    , quantity            = Nothing
    , grams               = Nothing
    , price               = Nothing
    , vendor              = Nothing
    , requires_shipping   = Nothing
    , taxable             = Nothing
    , fulfillment_service = Nothing
   }

instance FromJSON CarrierServiceItem where
  parseJSON = withObject "CarrierServiceItem" $ \o -> do
    name                <- o .:? "name"
    sku                 <- o .:? "sku"
    quantity            <- o .:? "quantity"
    grams               <- o .:? "grams"
    price               <- o .:? "price"
    vendor              <- o .:? "vendor"
    requires_shipping   <- o .:? "requires_shipping"
    taxable             <- o .:? "taxable"
    fulfillment_service <- o .:? "fulfillment_service"
    return CarrierServiceItem{..}


instance ToJSON CarrierServiceItem where
  toJSON CarrierServiceItem{..} = objectSansNull
    [ "name"                .= name
    , "sku"                 .= sku
    , "quantity"            .= quantity
    , "grams"               .= grams
    , "price"               .= price
    , "vendor"              .= vendor
    , "requires_shipping"   .= requires_shipping
    , "taxable"             .= taxable
    , "fulfillment_service" .= fulfillment_service
    ]


instance ToURLArgs CarrierServiceItem where
  toURLArgs CarrierServiceItem{..} = intercalate "&" $ filter (/= "")
    [ case name of
        Nothing -> ""
        Just x  -> Data.Text.concat ["carrier_service_item[name]=", pack $ show x]
    , case sku of
        Nothing -> ""
        Just x  -> Data.Text.concat ["carrier_service_item[sku]=", pack $ show x]
    , case quantity of
        Nothing -> ""
        Just x  -> Data.Text.concat ["carrier_service_item[quantity]=", pack $ show x]
    , case grams of
        Nothing -> ""
        Just x  -> Data.Text.concat ["carrier_service_item[grams]=", pack $ show x]
    , case price of
        Nothing -> ""
        Just x  -> Data.Text.concat ["carrier_service_item[price]=", pack $ show x]
    , case vendor of
        Nothing -> ""
        Just x  -> Data.Text.concat ["carrier_service_item[vendor]=", pack $ show x]
    , case requires_shipping of
        Nothing -> ""
        Just x  -> Data.Text.concat ["carrier_service_item[requires_shipping]=", pack $ show x]
    , case taxable of
        Nothing -> ""
        Just x  -> Data.Text.concat ["carrier_service_item[taxable]=", pack $ show x]
    , case fulfillment_service of
        Nothing -> ""
        Just x  -> Data.Text.concat ["carrier_service_item[fulfillment_service]=", pack $ show x]
    ]

instance HasName CarrierServiceItem where
  theNameMaybe = name                
  setName x y = y { name = Just x }

instance HasSKU CarrierServiceItem where
  theSKUMaybe = sku                 
  setSKU x y = y { sku = Just x }

instance HasQuantity CarrierServiceItem where
  theQuantityMaybe = quantity            
  setQuantity x y = y { quantity = Just x }

instance HasGrams CarrierServiceItem where
  theGramsMaybe = grams               
  setGrams x y = y { grams = Just x }

instance HasPriceInt CarrierServiceItem where
  thePriceIntMaybe = price               
  setPriceInt x y = y { price = Just x }

instance HasVendor CarrierServiceItem where
  theVendorMaybe = vendor              
  setVendor x y = y { vendor = Just x }

instance HasRequiresShipping CarrierServiceItem where
  theRequiresShippingMaybe = requires_shipping   
  setRequiresShipping x y = y { requires_shipping = Just x }

instance HasTaxable CarrierServiceItem where
  theTaxableMaybe = taxable             
  setTaxable x y = y { taxable = Just x }

instance HasFulfillmentServiceHandle CarrierServiceItem where
  theFulfillmentServiceHandleMaybe = fulfillment_service 
  setFulfillmentServiceHandle x y = y { fulfillment_service = Just x }

