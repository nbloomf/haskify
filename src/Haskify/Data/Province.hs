{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Haskify.Data.Province (
  Province()
  , HasProvince, theProvinceMaybe, setProvince, theProvince, HasProvinces, theProvincesMaybe, setProvinces, theProvinces
) where

import Prelude hiding (id, zip)

import Data.Text (Text, pack, concat, intercalate)
import Data.Aeson
import Data.Time.Clock (UTCTime)

import Haskify.Types
import Haskify.DataFields
import Haskify.JSON
import Haskify.HaskifyM

data Province = Province
 { code             :: Maybe Text
 , country_id       :: Maybe IDNumber
 , id               :: Maybe IDNumber
 , name             :: Maybe Text
 , shipping_zone_id :: Maybe IDNumber
 , tax              :: Maybe Float
 , tax_name         :: Maybe Text
 , tax_type         :: Maybe Text
 , tax_percentage   :: Maybe Float
 } deriving (Eq, Show)

class HasProvince t where
  theProvinceMaybe :: t -> Maybe Province
  setProvince :: Province -> t -> t

  theProvince :: t -> HaskifyM Province
  theProvince x = case theProvinceMaybe x of
    Nothing -> fieldDNE "Province"
    Just y -> return y

class HasProvinces t where
  theProvincesMaybe :: t -> Maybe [Province]
  setProvinces :: [Province] -> t -> t

  theProvinces :: t -> HaskifyM [Province]
  theProvinces x = case theProvincesMaybe x of
    Nothing -> fieldDNE "Province"
    Just y -> return y

instance NullObject Province where
  nullObject = Province
    { code             = Nothing
    , country_id       = Nothing
    , id               = Nothing
    , name             = Nothing
    , shipping_zone_id = Nothing
    , tax              = Nothing
    , tax_name         = Nothing
    , tax_type         = Nothing
    , tax_percentage   = Nothing
   }

instance FromJSON Province where
  parseJSON = withObject "Province" $ \o -> do
    code             <- o .:? "code"
    country_id       <- o .:? "country_id"
    id               <- o .:? "id"
    name             <- o .:? "name"
    shipping_zone_id <- o .:? "shipping_zone_id"
    tax              <- o .:? "tax"
    tax_name         <- o .:? "tax_name"
    tax_type         <- o .:? "tax_type"
    tax_percentage   <- o .:? "tax_percentage"
    return Province{..}


instance ToJSON Province where
  toJSON Province{..} = objectSansNull
    [ "code"             .= code
    , "country_id"       .= country_id
    , "id"               .= id
    , "name"             .= name
    , "shipping_zone_id" .= shipping_zone_id
    , "tax"              .= tax
    , "tax_name"         .= tax_name
    , "tax_type"         .= tax_type
    , "tax_percentage"   .= tax_percentage
    ]


instance ToURLArgs Province where
  toURLArgs Province{..} = intercalate "&" $ filter (/= "")
    [ case code of
        Nothing -> ""
        Just x  -> Data.Text.concat ["province[code]=", pack $ show x]
    , case country_id of
        Nothing -> ""
        Just x  -> Data.Text.concat ["province[country_id]=", pack $ show x]
    , case id of
        Nothing -> ""
        Just x  -> Data.Text.concat ["province[id]=", pack $ show x]
    , case name of
        Nothing -> ""
        Just x  -> Data.Text.concat ["province[name]=", pack $ show x]
    , case shipping_zone_id of
        Nothing -> ""
        Just x  -> Data.Text.concat ["province[shipping_zone_id]=", pack $ show x]
    , case tax of
        Nothing -> ""
        Just x  -> Data.Text.concat ["province[tax]=", pack $ show x]
    , case tax_name of
        Nothing -> ""
        Just x  -> Data.Text.concat ["province[tax_name]=", pack $ show x]
    , case tax_type of
        Nothing -> ""
        Just x  -> Data.Text.concat ["province[tax_type]=", pack $ show x]
    , case tax_percentage of
        Nothing -> ""
        Just x  -> Data.Text.concat ["province[tax_percentage]=", pack $ show x]
    ]

instance HasCode Province where
  theCodeMaybe = code             
  setCode x y = y { code = Just x }

instance HasCountryID Province where
  theCountryIDMaybe = country_id       
  setCountryID x y = y { country_id = Just x }

instance HasID Province where
  theIDMaybe = id               
  setID x y = y { id = Just x }

instance HasName Province where
  theNameMaybe = name             
  setName x y = y { name = Just x }

instance HasShippingZoneID Province where
  theShippingZoneIDMaybe = shipping_zone_id 
  setShippingZoneID x y = y { shipping_zone_id = Just x }

instance HasTax Province where
  theTaxMaybe = tax              
  setTax x y = y { tax = Just x }

instance HasTaxName Province where
  theTaxNameMaybe = tax_name         
  setTaxName x y = y { tax_name = Just x }

instance HasTaxType Province where
  theTaxTypeMaybe = tax_type         
  setTaxType x y = y { tax_type = Just x }

instance HasTaxPercentage Province where
  theTaxPercentageMaybe = tax_percentage   
  setTaxPercentage x y = y { tax_percentage = Just x }

