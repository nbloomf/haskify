{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Haskify.Data.CarrierServiceRate (
  CarrierServiceRate()
  , HasCarrierServiceRate, theCarrierServiceRateMaybe, setCarrierServiceRate, theCarrierServiceRate, HasCarrierServiceRates, theCarrierServiceRatesMaybe, setCarrierServiceRates, theCarrierServiceRates
) where

import Prelude hiding (id, zip)

import Data.Text (Text, pack, concat, intercalate)
import Data.Aeson
import Data.Time.Clock (UTCTime)

import Haskify.Types
import Haskify.DataFields
import Haskify.JSON
import Haskify.HaskifyM

data CarrierServiceRate = CarrierServiceRate
 { service_name      :: Maybe Text
 , service_code      :: Maybe Text
 , total_price       :: Maybe Text
 , currency          :: Maybe Text
 , min_delivery_date :: Maybe DateTime
 , max_delivery_date :: Maybe DateTime
 } deriving (Eq, Show)

class HasCarrierServiceRate t where
  theCarrierServiceRateMaybe :: t -> Maybe CarrierServiceRate
  setCarrierServiceRate :: CarrierServiceRate -> t -> t

  theCarrierServiceRate :: t -> HaskifyM CarrierServiceRate
  theCarrierServiceRate x = case theCarrierServiceRateMaybe x of
    Nothing -> fieldDNE "CarrierServiceRate"
    Just y -> return y

class HasCarrierServiceRates t where
  theCarrierServiceRatesMaybe :: t -> Maybe [CarrierServiceRate]
  setCarrierServiceRates :: [CarrierServiceRate] -> t -> t

  theCarrierServiceRates :: t -> HaskifyM [CarrierServiceRate]
  theCarrierServiceRates x = case theCarrierServiceRatesMaybe x of
    Nothing -> fieldDNE "CarrierServiceRate"
    Just y -> return y

instance NullObject CarrierServiceRate where
  nullObject = CarrierServiceRate
    { service_name      = Nothing
    , service_code      = Nothing
    , total_price       = Nothing
    , currency          = Nothing
    , min_delivery_date = Nothing
    , max_delivery_date = Nothing
   }

instance FromJSON CarrierServiceRate where
  parseJSON = withObject "CarrierServiceRate" $ \o -> do
    service_name      <- o .:? "service_name"
    service_code      <- o .:? "service_code"
    total_price       <- o .:? "total_price"
    currency          <- o .:? "currency"
    min_delivery_date <- o .:? "min_delivery_date"
    max_delivery_date <- o .:? "max_delivery_date"
    return CarrierServiceRate{..}


instance ToJSON CarrierServiceRate where
  toJSON CarrierServiceRate{..} = objectSansNull
    [ "service_name"      .= service_name
    , "service_code"      .= service_code
    , "total_price"       .= total_price
    , "currency"          .= currency
    , "min_delivery_date" .= min_delivery_date
    , "max_delivery_date" .= max_delivery_date
    ]


instance ToURLArgs CarrierServiceRate where
  toURLArgs CarrierServiceRate{..} = intercalate "&" $ filter (/= "")
    [ case service_name of
        Nothing -> ""
        Just x  -> Data.Text.concat ["carrier_service_rate[service_name]=", pack $ show x]
    , case service_code of
        Nothing -> ""
        Just x  -> Data.Text.concat ["carrier_service_rate[service_code]=", pack $ show x]
    , case total_price of
        Nothing -> ""
        Just x  -> Data.Text.concat ["carrier_service_rate[total_price]=", pack $ show x]
    , case currency of
        Nothing -> ""
        Just x  -> Data.Text.concat ["carrier_service_rate[currency]=", pack $ show x]
    , case min_delivery_date of
        Nothing -> ""
        Just x  -> Data.Text.concat ["carrier_service_rate[min_delivery_date]=", pack $ show x]
    , case max_delivery_date of
        Nothing -> ""
        Just x  -> Data.Text.concat ["carrier_service_rate[max_delivery_date]=", pack $ show x]
    ]

instance HasServiceName CarrierServiceRate where
  theServiceNameMaybe = service_name      
  setServiceName x y = y { service_name = Just x }

instance HasServiceCode CarrierServiceRate where
  theServiceCodeMaybe = service_code      
  setServiceCode x y = y { service_code = Just x }

instance HasTotalPrice CarrierServiceRate where
  theTotalPriceMaybe = total_price       
  setTotalPrice x y = y { total_price = Just x }

instance HasCurrency CarrierServiceRate where
  theCurrencyMaybe = currency          
  setCurrency x y = y { currency = Just x }

instance HasMinDeliveryDate CarrierServiceRate where
  theMinDeliveryDateMaybe = min_delivery_date 
  setMinDeliveryDate x y = y { min_delivery_date = Just x }

instance HasMaxDeliveryDate CarrierServiceRate where
  theMaxDeliveryDateMaybe = max_delivery_date 
  setMaxDeliveryDate x y = y { max_delivery_date = Just x }

