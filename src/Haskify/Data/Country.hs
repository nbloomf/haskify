{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Haskify.Data.Country (
  Country()
  , HasCountry, theCountryMaybe, setCountry, theCountry, HasCountrys, theCountrysMaybe, setCountrys, theCountrys
) where

import Prelude hiding (id, zip)

import Data.Text (Text, pack, concat, intercalate)
import Data.Aeson
import Data.Time.Clock (UTCTime)

import Haskify.Types
import Haskify.DataFields
import Haskify.JSON
import Haskify.HaskifyM
import Haskify.Data.Province

data Country = Country
 { code      :: Maybe Text
 , id        :: Maybe IDNumber
 , name      :: Maybe Text
 , provinces :: Maybe [Province]
 , tax       :: Maybe Float
 } deriving (Eq, Show)

class HasCountry t where
  theCountryMaybe :: t -> Maybe Country
  setCountry :: Country -> t -> t

  theCountry :: t -> HaskifyM Country
  theCountry x = case theCountryMaybe x of
    Nothing -> fieldDNE "Country"
    Just y -> return y

class HasCountrys t where
  theCountrysMaybe :: t -> Maybe [Country]
  setCountrys :: [Country] -> t -> t

  theCountrys :: t -> HaskifyM [Country]
  theCountrys x = case theCountrysMaybe x of
    Nothing -> fieldDNE "Country"
    Just y -> return y

instance NullObject Country where
  nullObject = Country
    { code      = Nothing
    , id        = Nothing
    , name      = Nothing
    , provinces = Nothing
    , tax       = Nothing
   }

instance FromJSON Country where
  parseJSON = withObject "Country" $ \o -> do
    code      <- o .:? "code"
    id        <- o .:? "id"
    name      <- o .:? "name"
    provinces <- o .:? "provinces"
    tax       <- o .:? "tax"
    return Country{..}


instance ToJSON Country where
  toJSON Country{..} = objectSansNull
    [ "code"      .= code
    , "id"        .= id
    , "name"      .= name
    , "provinces" .= provinces
    , "tax"       .= tax
    ]


instance ToURLArgs Country where
  toURLArgs Country{..} = intercalate "&" $ filter (/= "")
    [ case code of
        Nothing -> ""
        Just x  -> Data.Text.concat ["country[code]=", pack $ show x]
    , case id of
        Nothing -> ""
        Just x  -> Data.Text.concat ["country[id]=", pack $ show x]
    , case name of
        Nothing -> ""
        Just x  -> Data.Text.concat ["country[name]=", pack $ show x]
    , case provinces of
        Nothing -> ""
        Just x  -> Data.Text.concat ["country[provinces]=", pack $ show x]
    , case tax of
        Nothing -> ""
        Just x  -> Data.Text.concat ["country[tax]=", pack $ show x]
    ]

instance HasCode Country where
  theCodeMaybe = code      
  setCode x y = y { code = Just x }

instance HasID Country where
  theIDMaybe = id        
  setID x y = y { id = Just x }

instance HasName Country where
  theNameMaybe = name      
  setName x y = y { name = Just x }

instance HasProvinces Country where
  theProvincesMaybe = provinces 
  setProvinces x y = y { provinces = Just x }

instance HasTax Country where
  theTaxMaybe = tax       
  setTax x y = y { tax = Just x }

