{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Haskify.Data.CarrierServiceRequest (
  CarrierServiceRequest()
  , HasCarrierServiceRequest, theCarrierServiceRequestMaybe, setCarrierServiceRequest, theCarrierServiceRequest, HasCarrierServiceRequests, theCarrierServiceRequestsMaybe, setCarrierServiceRequests, theCarrierServiceRequests
) where

import Prelude hiding (id, zip)

import Data.Text (Text, pack, concat, intercalate)
import Data.Aeson
import Data.Time.Clock (UTCTime)

import Haskify.Types
import Haskify.DataFields
import Haskify.JSON
import Haskify.HaskifyM
import Haskify.Data.CarrierServiceLocation
import Haskify.Data.CarrierServiceItem

data CarrierServiceRequest = CarrierServiceRequest
 { origin      :: Maybe CarrierServiceLocation
 , destination :: Maybe CarrierServiceLocation
 , items       :: Maybe [CarrierServiceItem]
 , currency    :: Maybe Text
 } deriving (Eq, Show)

class HasCarrierServiceRequest t where
  theCarrierServiceRequestMaybe :: t -> Maybe CarrierServiceRequest
  setCarrierServiceRequest :: CarrierServiceRequest -> t -> t

  theCarrierServiceRequest :: t -> HaskifyM CarrierServiceRequest
  theCarrierServiceRequest x = case theCarrierServiceRequestMaybe x of
    Nothing -> fieldDNE "CarrierServiceRequest"
    Just y -> return y

class HasCarrierServiceRequests t where
  theCarrierServiceRequestsMaybe :: t -> Maybe [CarrierServiceRequest]
  setCarrierServiceRequests :: [CarrierServiceRequest] -> t -> t

  theCarrierServiceRequests :: t -> HaskifyM [CarrierServiceRequest]
  theCarrierServiceRequests x = case theCarrierServiceRequestsMaybe x of
    Nothing -> fieldDNE "CarrierServiceRequest"
    Just y -> return y

instance NullObject CarrierServiceRequest where
  nullObject = CarrierServiceRequest
    { origin      = Nothing
    , destination = Nothing
    , items       = Nothing
    , currency    = Nothing
   }

instance FromJSON CarrierServiceRequest where
  parseJSON = withObject "CarrierServiceRequest" $ \o -> do
    origin      <- o .:? "origin"
    destination <- o .:? "destination"
    items       <- o .:? "items"
    currency    <- o .:? "currency"
    return CarrierServiceRequest{..}


instance ToJSON CarrierServiceRequest where
  toJSON CarrierServiceRequest{..} = objectSansNull
    [ "origin"      .= origin
    , "destination" .= destination
    , "items"       .= items
    , "currency"    .= currency
    ]


instance ToURLArgs CarrierServiceRequest where
  toURLArgs CarrierServiceRequest{..} = intercalate "&" $ filter (/= "")
    [ case origin of
        Nothing -> ""
        Just x  -> Data.Text.concat ["carrier_service_request[origin]=", pack $ show x]
    , case destination of
        Nothing -> ""
        Just x  -> Data.Text.concat ["carrier_service_request[destination]=", pack $ show x]
    , case items of
        Nothing -> ""
        Just x  -> Data.Text.concat ["carrier_service_request[items]=", pack $ show x]
    , case currency of
        Nothing -> ""
        Just x  -> Data.Text.concat ["carrier_service_request[currency]=", pack $ show x]
    ]

instance HasOrigin CarrierServiceRequest where
  theOriginMaybe = origin      
  setOrigin x y = y { origin = Just x }

instance HasDestination CarrierServiceRequest where
  theDestinationMaybe = destination 
  setDestination x y = y { destination = Just x }

instance HasCarrierServiceItems CarrierServiceRequest where
  theCarrierServiceItemsMaybe = items       
  setCarrierServiceItems x y = y { items = Just x }

instance HasCurrency CarrierServiceRequest where
  theCurrencyMaybe = currency    
  setCurrency x y = y { currency = Just x }

