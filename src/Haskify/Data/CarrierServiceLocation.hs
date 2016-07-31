{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Haskify.Data.CarrierServiceLocation (
  CarrierServiceLocation()
  , HasCarrierServiceLocation, theCarrierServiceLocationMaybe, setCarrierServiceLocation, theCarrierServiceLocation, HasCarrierServiceLocations, theCarrierServiceLocationsMaybe, setCarrierServiceLocations, theCarrierServiceLocations
, HasOrigin, theOriginMaybe, setOrigin, theOrigin
, HasDestination, theDestinationMaybe, setDestination, theDestination
) where

import Prelude hiding (id, zip)

import Data.Text (Text, pack, concat, intercalate)
import Data.Aeson
import Data.Time.Clock (UTCTime)

import Haskify.Types
import Haskify.DataFields
import Haskify.JSON
import Haskify.HaskifyM

data CarrierServiceLocation = CarrierServiceLocation
 { country      :: Maybe Text
 , postal_code  :: Maybe Text
 , province     :: Maybe Text
 , city         :: Maybe Text
 , name         :: Maybe Text
 , address1     :: Maybe Text
 , address2     :: Maybe Text
 , address3     :: Maybe Text
 , phone        :: Maybe Text
 , fax          :: Maybe Text
 , address_type :: Maybe Text
 , company_name :: Maybe Text
 } deriving (Eq, Show)

class HasCarrierServiceLocation t where
  theCarrierServiceLocationMaybe :: t -> Maybe CarrierServiceLocation
  setCarrierServiceLocation :: CarrierServiceLocation -> t -> t

  theCarrierServiceLocation :: t -> HaskifyM CarrierServiceLocation
  theCarrierServiceLocation x = case theCarrierServiceLocationMaybe x of
    Nothing -> fieldDNE "CarrierServiceLocation"
    Just y -> return y

class HasCarrierServiceLocations t where
  theCarrierServiceLocationsMaybe :: t -> Maybe [CarrierServiceLocation]
  setCarrierServiceLocations :: [CarrierServiceLocation] -> t -> t

  theCarrierServiceLocations :: t -> HaskifyM [CarrierServiceLocation]
  theCarrierServiceLocations x = case theCarrierServiceLocationsMaybe x of
    Nothing -> fieldDNE "CarrierServiceLocation"
    Just y -> return y

class HasOrigin t where
  theOriginMaybe :: t -> Maybe CarrierServiceLocation
  setOrigin :: CarrierServiceLocation -> t -> t

  theOrigin :: t -> HaskifyM CarrierServiceLocation
  theOrigin x = case theOriginMaybe x of
    Nothing -> fieldDNE "Origin"
    Just y -> return y

class HasDestination t where
  theDestinationMaybe :: t -> Maybe CarrierServiceLocation
  setDestination :: CarrierServiceLocation -> t -> t

  theDestination :: t -> HaskifyM CarrierServiceLocation
  theDestination x = case theDestinationMaybe x of
    Nothing -> fieldDNE "Destination"
    Just y -> return y

instance NullObject CarrierServiceLocation where
  nullObject = CarrierServiceLocation
    { country      = Nothing
    , postal_code  = Nothing
    , province     = Nothing
    , city         = Nothing
    , name         = Nothing
    , address1     = Nothing
    , address2     = Nothing
    , address3     = Nothing
    , phone        = Nothing
    , fax          = Nothing
    , address_type = Nothing
    , company_name = Nothing
   }

instance FromJSON CarrierServiceLocation where
  parseJSON = withObject "CarrierServiceLocation" $ \o -> do
    country      <- o .:? "country"
    postal_code  <- o .:? "postal_code"
    province     <- o .:? "province"
    city         <- o .:? "city"
    name         <- o .:? "name"
    address1     <- o .:? "address1"
    address2     <- o .:? "address2"
    address3     <- o .:? "address3"
    phone        <- o .:? "phone"
    fax          <- o .:? "fax"
    address_type <- o .:? "address_type"
    company_name <- o .:? "company_name"
    return CarrierServiceLocation{..}


instance ToJSON CarrierServiceLocation where
  toJSON CarrierServiceLocation{..} = objectSansNull
    [ "country"      .= country
    , "postal_code"  .= postal_code
    , "province"     .= province
    , "city"         .= city
    , "name"         .= name
    , "address1"     .= address1
    , "address2"     .= address2
    , "address3"     .= address3
    , "phone"        .= phone
    , "fax"          .= fax
    , "address_type" .= address_type
    , "company_name" .= company_name
    ]


instance ToURLArgs CarrierServiceLocation where
  toURLArgs CarrierServiceLocation{..} = intercalate "&" $ filter (/= "")
    [ case country of
        Nothing -> ""
        Just x  -> Data.Text.concat ["carrier_service_location[country]=", pack $ show x]
    , case postal_code of
        Nothing -> ""
        Just x  -> Data.Text.concat ["carrier_service_location[postal_code]=", pack $ show x]
    , case province of
        Nothing -> ""
        Just x  -> Data.Text.concat ["carrier_service_location[province]=", pack $ show x]
    , case city of
        Nothing -> ""
        Just x  -> Data.Text.concat ["carrier_service_location[city]=", pack $ show x]
    , case name of
        Nothing -> ""
        Just x  -> Data.Text.concat ["carrier_service_location[name]=", pack $ show x]
    , case address1 of
        Nothing -> ""
        Just x  -> Data.Text.concat ["carrier_service_location[address1]=", pack $ show x]
    , case address2 of
        Nothing -> ""
        Just x  -> Data.Text.concat ["carrier_service_location[address2]=", pack $ show x]
    , case address3 of
        Nothing -> ""
        Just x  -> Data.Text.concat ["carrier_service_location[address3]=", pack $ show x]
    , case phone of
        Nothing -> ""
        Just x  -> Data.Text.concat ["carrier_service_location[phone]=", pack $ show x]
    , case fax of
        Nothing -> ""
        Just x  -> Data.Text.concat ["carrier_service_location[fax]=", pack $ show x]
    , case address_type of
        Nothing -> ""
        Just x  -> Data.Text.concat ["carrier_service_location[address_type]=", pack $ show x]
    , case company_name of
        Nothing -> ""
        Just x  -> Data.Text.concat ["carrier_service_location[company_name]=", pack $ show x]
    ]

instance HasCountryName CarrierServiceLocation where
  theCountryNameMaybe = country      
  setCountryName x y = y { country = Just x }

instance HasPostalCode CarrierServiceLocation where
  thePostalCodeMaybe = postal_code  
  setPostalCode x y = y { postal_code = Just x }

instance HasProvinceName CarrierServiceLocation where
  theProvinceNameMaybe = province     
  setProvinceName x y = y { province = Just x }

instance HasCity CarrierServiceLocation where
  theCityMaybe = city         
  setCity x y = y { city = Just x }

instance HasName CarrierServiceLocation where
  theNameMaybe = name         
  setName x y = y { name = Just x }

instance HasAddress1 CarrierServiceLocation where
  theAddress1Maybe = address1     
  setAddress1 x y = y { address1 = Just x }

instance HasAddress2 CarrierServiceLocation where
  theAddress2Maybe = address2     
  setAddress2 x y = y { address2 = Just x }

instance HasAddress3 CarrierServiceLocation where
  theAddress3Maybe = address3     
  setAddress3 x y = y { address3 = Just x }

instance HasPhone CarrierServiceLocation where
  thePhoneMaybe = phone        
  setPhone x y = y { phone = Just x }

instance HasFax CarrierServiceLocation where
  theFaxMaybe = fax          
  setFax x y = y { fax = Just x }

instance HasAddressType CarrierServiceLocation where
  theAddressTypeMaybe = address_type 
  setAddressType x y = y { address_type = Just x }

instance HasCompanyName CarrierServiceLocation where
  theCompanyNameMaybe = company_name 
  setCompanyName x y = y { company_name = Just x }

