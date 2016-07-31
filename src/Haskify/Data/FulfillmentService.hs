{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Haskify.Data.FulfillmentService (
  FulfillmentService()
  , HasFulfillmentService, theFulfillmentServiceMaybe, setFulfillmentService, theFulfillmentService, HasFulfillmentServices, theFulfillmentServicesMaybe, setFulfillmentServices, theFulfillmentServices
) where

import Prelude hiding (id, zip)

import Data.Text (Text, pack, concat, intercalate)
import Data.Aeson
import Data.Time.Clock (UTCTime)

import Haskify.Types
import Haskify.DataFields
import Haskify.JSON
import Haskify.HaskifyM
import Haskify.Value.Format
import Haskify.Value.InventoryManagement

data FulfillmentService = FulfillmentService
 { callback_url             :: Maybe Text
 , format                   :: Maybe Format
 , handle                   :: Maybe Text
 , inventory_management     :: Maybe InventoryManagement
 , name                     :: Maybe Text
 , provider_id              :: Maybe IDNumber
 , requires_shipping_method :: Maybe Bool
 , tracking_support         :: Maybe Bool
 } deriving (Eq, Show)

class HasFulfillmentService t where
  theFulfillmentServiceMaybe :: t -> Maybe FulfillmentService
  setFulfillmentService :: FulfillmentService -> t -> t

  theFulfillmentService :: t -> HaskifyM FulfillmentService
  theFulfillmentService x = case theFulfillmentServiceMaybe x of
    Nothing -> fieldDNE "FulfillmentService"
    Just y -> return y

class HasFulfillmentServices t where
  theFulfillmentServicesMaybe :: t -> Maybe [FulfillmentService]
  setFulfillmentServices :: [FulfillmentService] -> t -> t

  theFulfillmentServices :: t -> HaskifyM [FulfillmentService]
  theFulfillmentServices x = case theFulfillmentServicesMaybe x of
    Nothing -> fieldDNE "FulfillmentService"
    Just y -> return y

instance NullObject FulfillmentService where
  nullObject = FulfillmentService
    { callback_url             = Nothing
    , format                   = Nothing
    , handle                   = Nothing
    , inventory_management     = Nothing
    , name                     = Nothing
    , provider_id              = Nothing
    , requires_shipping_method = Nothing
    , tracking_support         = Nothing
   }

instance FromJSON FulfillmentService where
  parseJSON = withObject "FulfillmentService" $ \o -> do
    callback_url             <- o .:? "callback_url"
    format                   <- o .:? "format"
    handle                   <- o .:? "handle"
    inventory_management     <- o .:? "inventory_management"
    name                     <- o .:? "name"
    provider_id              <- o .:? "provider_id"
    requires_shipping_method <- o .:? "requires_shipping_method"
    tracking_support         <- o .:? "tracking_support"
    return FulfillmentService{..}


instance ToJSON FulfillmentService where
  toJSON FulfillmentService{..} = objectSansNull
    [ "callback_url"             .= callback_url
    , "format"                   .= format
    , "handle"                   .= handle
    , "inventory_management"     .= inventory_management
    , "name"                     .= name
    , "provider_id"              .= provider_id
    , "requires_shipping_method" .= requires_shipping_method
    , "tracking_support"         .= tracking_support
    ]


instance ToURLArgs FulfillmentService where
  toURLArgs FulfillmentService{..} = intercalate "&" $ filter (/= "")
    [ case callback_url of
        Nothing -> ""
        Just x  -> Data.Text.concat ["fulfillment_service[callback_url]=", pack $ show x]
    , case format of
        Nothing -> ""
        Just x  -> Data.Text.concat ["fulfillment_service[format]=", pack $ show x]
    , case handle of
        Nothing -> ""
        Just x  -> Data.Text.concat ["fulfillment_service[handle]=", pack $ show x]
    , case inventory_management of
        Nothing -> ""
        Just x  -> Data.Text.concat ["fulfillment_service[inventory_management]=", pack $ show x]
    , case name of
        Nothing -> ""
        Just x  -> Data.Text.concat ["fulfillment_service[name]=", pack $ show x]
    , case provider_id of
        Nothing -> ""
        Just x  -> Data.Text.concat ["fulfillment_service[provider_id]=", pack $ show x]
    , case requires_shipping_method of
        Nothing -> ""
        Just x  -> Data.Text.concat ["fulfillment_service[requires_shipping_method]=", pack $ show x]
    , case tracking_support of
        Nothing -> ""
        Just x  -> Data.Text.concat ["fulfillment_service[tracking_support]=", pack $ show x]
    ]

instance HasCallbackURL FulfillmentService where
  theCallbackURLMaybe = callback_url             
  setCallbackURL x y = y { callback_url = Just x }

instance HasFormat FulfillmentService where
  theFormatMaybe = format                   
  setFormat x y = y { format = Just x }

instance HasHandle FulfillmentService where
  theHandleMaybe = handle                   
  setHandle x y = y { handle = Just x }

instance HasInventoryManagement FulfillmentService where
  theInventoryManagementMaybe = inventory_management     
  setInventoryManagement x y = y { inventory_management = Just x }

instance HasName FulfillmentService where
  theNameMaybe = name                     
  setName x y = y { name = Just x }

instance HasProviderID FulfillmentService where
  theProviderIDMaybe = provider_id              
  setProviderID x y = y { provider_id = Just x }

instance HasRequiresShippingMethod FulfillmentService where
  theRequiresShippingMethodMaybe = requires_shipping_method 
  setRequiresShippingMethod x y = y { requires_shipping_method = Just x }

instance HasTrackingSupport FulfillmentService where
  theTrackingSupportMaybe = tracking_support         
  setTrackingSupport x y = y { tracking_support = Just x }

