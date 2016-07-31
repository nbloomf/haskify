{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Haskify.Data.CarrierService (
  CarrierService()
  , HasCarrierService, theCarrierServiceMaybe, setCarrierService, theCarrierService, HasCarrierServices, theCarrierServicesMaybe, setCarrierServices, theCarrierServices
) where

import Prelude hiding (id, zip)

import Data.Text (Text, pack, concat, intercalate)
import Data.Aeson
import Data.Time.Clock (UTCTime)

import Haskify.Types
import Haskify.DataFields
import Haskify.JSON
import Haskify.HaskifyM
import Haskify.Value.CarrierServiceType
import Haskify.Value.Format

data CarrierService = CarrierService
 { active               :: Maybe Bool
 , callback_url         :: Maybe Text
 , carrier_service_type :: Maybe CarrierServiceType
 , format               :: Maybe Format
 , id                   :: Maybe IDNumber
 , name                 :: Maybe Text
 , service_discovery    :: Maybe Bool
 } deriving (Eq, Show)

class HasCarrierService t where
  theCarrierServiceMaybe :: t -> Maybe CarrierService
  setCarrierService :: CarrierService -> t -> t

  theCarrierService :: t -> HaskifyM CarrierService
  theCarrierService x = case theCarrierServiceMaybe x of
    Nothing -> fieldDNE "CarrierService"
    Just y -> return y

class HasCarrierServices t where
  theCarrierServicesMaybe :: t -> Maybe [CarrierService]
  setCarrierServices :: [CarrierService] -> t -> t

  theCarrierServices :: t -> HaskifyM [CarrierService]
  theCarrierServices x = case theCarrierServicesMaybe x of
    Nothing -> fieldDNE "CarrierService"
    Just y -> return y

instance NullObject CarrierService where
  nullObject = CarrierService
    { active               = Nothing
    , callback_url         = Nothing
    , carrier_service_type = Nothing
    , format               = Nothing
    , id                   = Nothing
    , name                 = Nothing
    , service_discovery    = Nothing
   }

instance FromJSON CarrierService where
  parseJSON = withObject "CarrierService" $ \o -> do
    active               <- o .:? "active"
    callback_url         <- o .:? "callback_url"
    carrier_service_type <- o .:? "carrier_service_type"
    format               <- o .:? "format"
    id                   <- o .:? "id"
    name                 <- o .:? "name"
    service_discovery    <- o .:? "service_discovery"
    return CarrierService{..}


instance ToJSON CarrierService where
  toJSON CarrierService{..} = objectSansNull
    [ "active"               .= active
    , "callback_url"         .= callback_url
    , "carrier_service_type" .= carrier_service_type
    , "format"               .= format
    , "id"                   .= id
    , "name"                 .= name
    , "service_discovery"    .= service_discovery
    ]


instance ToURLArgs CarrierService where
  toURLArgs CarrierService{..} = intercalate "&" $ filter (/= "")
    [ case active of
        Nothing -> ""
        Just x  -> Data.Text.concat ["carrier_service[active]=", pack $ show x]
    , case callback_url of
        Nothing -> ""
        Just x  -> Data.Text.concat ["carrier_service[callback_url]=", pack $ show x]
    , case carrier_service_type of
        Nothing -> ""
        Just x  -> Data.Text.concat ["carrier_service[carrier_service_type]=", pack $ show x]
    , case format of
        Nothing -> ""
        Just x  -> Data.Text.concat ["carrier_service[format]=", pack $ show x]
    , case id of
        Nothing -> ""
        Just x  -> Data.Text.concat ["carrier_service[id]=", pack $ show x]
    , case name of
        Nothing -> ""
        Just x  -> Data.Text.concat ["carrier_service[name]=", pack $ show x]
    , case service_discovery of
        Nothing -> ""
        Just x  -> Data.Text.concat ["carrier_service[service_discovery]=", pack $ show x]
    ]

instance HasActive CarrierService where
  theActiveMaybe = active               
  setActive x y = y { active = Just x }

instance HasCallbackURL CarrierService where
  theCallbackURLMaybe = callback_url         
  setCallbackURL x y = y { callback_url = Just x }

instance HasCarrierServiceType CarrierService where
  theCarrierServiceTypeMaybe = carrier_service_type 
  setCarrierServiceType x y = y { carrier_service_type = Just x }

instance HasFormat CarrierService where
  theFormatMaybe = format               
  setFormat x y = y { format = Just x }

instance HasID CarrierService where
  theIDMaybe = id                   
  setID x y = y { id = Just x }

instance HasName CarrierService where
  theNameMaybe = name                 
  setName x y = y { name = Just x }

instance HasServiceDiscovery CarrierService where
  theServiceDiscoveryMaybe = service_discovery    
  setServiceDiscovery x y = y { service_discovery = Just x }

