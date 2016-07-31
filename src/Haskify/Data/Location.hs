{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Haskify.Data.Location (
  Location()
  , HasLocation, theLocationMaybe, setLocation, theLocation, HasLocations, theLocationsMaybe, setLocations, theLocations
) where

import Prelude hiding (id, zip)

import Data.Text (Text, pack, concat, intercalate)
import Data.Aeson
import Data.Time.Clock (UTCTime)

import Haskify.Types
import Haskify.DataFields
import Haskify.JSON
import Haskify.HaskifyM

data Location = Location
 { id         :: Maybe IDNumber
 , name       :: Maybe Text
 , address1   :: Maybe Text
 , address2   :: Maybe Text
 , zip        :: Maybe Text
 , city       :: Maybe Text
 , province   :: Maybe Text
 , country    :: Maybe Text
 , phone      :: Maybe Text
 , created_at :: Maybe DateTime
 , updated_at :: Maybe DateTime
 } deriving (Eq, Show)

class HasLocation t where
  theLocationMaybe :: t -> Maybe Location
  setLocation :: Location -> t -> t

  theLocation :: t -> HaskifyM Location
  theLocation x = case theLocationMaybe x of
    Nothing -> fieldDNE "Location"
    Just y -> return y

class HasLocations t where
  theLocationsMaybe :: t -> Maybe [Location]
  setLocations :: [Location] -> t -> t

  theLocations :: t -> HaskifyM [Location]
  theLocations x = case theLocationsMaybe x of
    Nothing -> fieldDNE "Location"
    Just y -> return y

instance NullObject Location where
  nullObject = Location
    { id         = Nothing
    , name       = Nothing
    , address1   = Nothing
    , address2   = Nothing
    , zip        = Nothing
    , city       = Nothing
    , province   = Nothing
    , country    = Nothing
    , phone      = Nothing
    , created_at = Nothing
    , updated_at = Nothing
   }

instance FromJSON Location where
  parseJSON = withObject "Location" $ \o -> do
    id         <- o .:? "id"
    name       <- o .:? "name"
    address1   <- o .:? "address1"
    address2   <- o .:? "address2"
    zip        <- o .:? "zip"
    city       <- o .:? "city"
    province   <- o .:? "province"
    country    <- o .:? "country"
    phone      <- o .:? "phone"
    created_at <- o .:? "created_at"
    updated_at <- o .:? "updated_at"
    return Location{..}


instance ToJSON Location where
  toJSON Location{..} = objectSansNull
    [ "id"         .= id
    , "name"       .= name
    , "address1"   .= address1
    , "address2"   .= address2
    , "zip"        .= zip
    , "city"       .= city
    , "province"   .= province
    , "country"    .= country
    , "phone"      .= phone
    , "created_at" .= created_at
    , "updated_at" .= updated_at
    ]


instance ToURLArgs Location where
  toURLArgs Location{..} = intercalate "&" $ filter (/= "")
    [ case id of
        Nothing -> ""
        Just x  -> Data.Text.concat ["location[id]=", pack $ show x]
    , case name of
        Nothing -> ""
        Just x  -> Data.Text.concat ["location[name]=", pack $ show x]
    , case address1 of
        Nothing -> ""
        Just x  -> Data.Text.concat ["location[address1]=", pack $ show x]
    , case address2 of
        Nothing -> ""
        Just x  -> Data.Text.concat ["location[address2]=", pack $ show x]
    , case zip of
        Nothing -> ""
        Just x  -> Data.Text.concat ["location[zip]=", pack $ show x]
    , case city of
        Nothing -> ""
        Just x  -> Data.Text.concat ["location[city]=", pack $ show x]
    , case province of
        Nothing -> ""
        Just x  -> Data.Text.concat ["location[province]=", pack $ show x]
    , case country of
        Nothing -> ""
        Just x  -> Data.Text.concat ["location[country]=", pack $ show x]
    , case phone of
        Nothing -> ""
        Just x  -> Data.Text.concat ["location[phone]=", pack $ show x]
    , case created_at of
        Nothing -> ""
        Just x  -> Data.Text.concat ["location[created_at]=", pack $ show x]
    , case updated_at of
        Nothing -> ""
        Just x  -> Data.Text.concat ["location[updated_at]=", pack $ show x]
    ]

instance HasID Location where
  theIDMaybe = id         
  setID x y = y { id = Just x }

instance HasName Location where
  theNameMaybe = name       
  setName x y = y { name = Just x }

instance HasAddress1 Location where
  theAddress1Maybe = address1   
  setAddress1 x y = y { address1 = Just x }

instance HasAddress2 Location where
  theAddress2Maybe = address2   
  setAddress2 x y = y { address2 = Just x }

instance HasZip Location where
  theZipMaybe = zip        
  setZip x y = y { zip = Just x }

instance HasCity Location where
  theCityMaybe = city       
  setCity x y = y { city = Just x }

instance HasProvinceName Location where
  theProvinceNameMaybe = province   
  setProvinceName x y = y { province = Just x }

instance HasCountryText Location where
  theCountryTextMaybe = country    
  setCountryText x y = y { country = Just x }

instance HasPhone Location where
  thePhoneMaybe = phone      
  setPhone x y = y { phone = Just x }

instance HasCreatedAt Location where
  theCreatedAtMaybe = created_at 
  setCreatedAt x y = y { created_at = Just x }

instance HasUpdatedAt Location where
  theUpdatedAtMaybe = updated_at 
  setUpdatedAt x y = y { updated_at = Just x }

