{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Haskify.Data.CustomerAddress (
  CustomerAddress()
  , HasCustomerAddress, theCustomerAddressMaybe, setCustomerAddress, theCustomerAddress, HasCustomerAddresss, theCustomerAddresssMaybe, setCustomerAddresss, theCustomerAddresss
, HasDefaultAddress, theDefaultAddressMaybe, setDefaultAddress, theDefaultAddress
) where

import Prelude hiding (id, zip)

import Data.Text (Text, pack, concat, intercalate)
import Data.Aeson
import Data.Time.Clock (UTCTime)

import Haskify.Types
import Haskify.DataFields
import Haskify.JSON
import Haskify.HaskifyM

data CustomerAddress = CustomerAddress
 { address1      :: Maybe Text
 , address2      :: Maybe Text
 , city          :: Maybe Text
 , company       :: Maybe Text
 , first_name    :: Maybe Text
 , last_name     :: Maybe Text
 , phone         :: Maybe Text
 , province      :: Maybe Text
 , country       :: Maybe Text
 , zip           :: Maybe Text
 , name          :: Maybe Text
 , province_code :: Maybe Text
 , country_code  :: Maybe Text
 , country_name  :: Maybe Text
 } deriving (Eq, Show)

class HasCustomerAddress t where
  theCustomerAddressMaybe :: t -> Maybe CustomerAddress
  setCustomerAddress :: CustomerAddress -> t -> t

  theCustomerAddress :: t -> HaskifyM CustomerAddress
  theCustomerAddress x = case theCustomerAddressMaybe x of
    Nothing -> fieldDNE "CustomerAddress"
    Just y -> return y

class HasCustomerAddresss t where
  theCustomerAddresssMaybe :: t -> Maybe [CustomerAddress]
  setCustomerAddresss :: [CustomerAddress] -> t -> t

  theCustomerAddresss :: t -> HaskifyM [CustomerAddress]
  theCustomerAddresss x = case theCustomerAddresssMaybe x of
    Nothing -> fieldDNE "CustomerAddress"
    Just y -> return y

class HasDefaultAddress t where
  theDefaultAddressMaybe :: t -> Maybe CustomerAddress
  setDefaultAddress :: CustomerAddress -> t -> t

  theDefaultAddress :: t -> HaskifyM CustomerAddress
  theDefaultAddress x = case theDefaultAddressMaybe x of
    Nothing -> fieldDNE "DefaultAddress"
    Just y -> return y

instance NullObject CustomerAddress where
  nullObject = CustomerAddress
    { address1      = Nothing
    , address2      = Nothing
    , city          = Nothing
    , company       = Nothing
    , first_name    = Nothing
    , last_name     = Nothing
    , phone         = Nothing
    , province      = Nothing
    , country       = Nothing
    , zip           = Nothing
    , name          = Nothing
    , province_code = Nothing
    , country_code  = Nothing
    , country_name  = Nothing
   }

instance FromJSON CustomerAddress where
  parseJSON = withObject "CustomerAddress" $ \o -> do
    address1      <- o .:? "address1"
    address2      <- o .:? "address2"
    city          <- o .:? "city"
    company       <- o .:? "company"
    first_name    <- o .:? "first_name"
    last_name     <- o .:? "last_name"
    phone         <- o .:? "phone"
    province      <- o .:? "province"
    country       <- o .:? "country"
    zip           <- o .:? "zip"
    name          <- o .:? "name"
    province_code <- o .:? "province_code"
    country_code  <- o .:? "country_code"
    country_name  <- o .:? "country_name"
    return CustomerAddress{..}


instance ToJSON CustomerAddress where
  toJSON CustomerAddress{..} = objectSansNull
    [ "address1"      .= address1
    , "address2"      .= address2
    , "city"          .= city
    , "company"       .= company
    , "first_name"    .= first_name
    , "last_name"     .= last_name
    , "phone"         .= phone
    , "province"      .= province
    , "country"       .= country
    , "zip"           .= zip
    , "name"          .= name
    , "province_code" .= province_code
    , "country_code"  .= country_code
    , "country_name"  .= country_name
    ]


instance ToURLArgs CustomerAddress where
  toURLArgs CustomerAddress{..} = intercalate "&" $ filter (/= "")
    [ case address1 of
        Nothing -> ""
        Just x  -> Data.Text.concat ["customer_address[address1]=", pack $ show x]
    , case address2 of
        Nothing -> ""
        Just x  -> Data.Text.concat ["customer_address[address2]=", pack $ show x]
    , case city of
        Nothing -> ""
        Just x  -> Data.Text.concat ["customer_address[city]=", pack $ show x]
    , case company of
        Nothing -> ""
        Just x  -> Data.Text.concat ["customer_address[company]=", pack $ show x]
    , case first_name of
        Nothing -> ""
        Just x  -> Data.Text.concat ["customer_address[first_name]=", pack $ show x]
    , case last_name of
        Nothing -> ""
        Just x  -> Data.Text.concat ["customer_address[last_name]=", pack $ show x]
    , case phone of
        Nothing -> ""
        Just x  -> Data.Text.concat ["customer_address[phone]=", pack $ show x]
    , case province of
        Nothing -> ""
        Just x  -> Data.Text.concat ["customer_address[province]=", pack $ show x]
    , case country of
        Nothing -> ""
        Just x  -> Data.Text.concat ["customer_address[country]=", pack $ show x]
    , case zip of
        Nothing -> ""
        Just x  -> Data.Text.concat ["customer_address[zip]=", pack $ show x]
    , case name of
        Nothing -> ""
        Just x  -> Data.Text.concat ["customer_address[name]=", pack $ show x]
    , case province_code of
        Nothing -> ""
        Just x  -> Data.Text.concat ["customer_address[province_code]=", pack $ show x]
    , case country_code of
        Nothing -> ""
        Just x  -> Data.Text.concat ["customer_address[country_code]=", pack $ show x]
    , case country_name of
        Nothing -> ""
        Just x  -> Data.Text.concat ["customer_address[country_name]=", pack $ show x]
    ]

instance HasAddress1 CustomerAddress where
  theAddress1Maybe = address1      
  setAddress1 x y = y { address1 = Just x }

instance HasAddress2 CustomerAddress where
  theAddress2Maybe = address2      
  setAddress2 x y = y { address2 = Just x }

instance HasCity CustomerAddress where
  theCityMaybe = city          
  setCity x y = y { city = Just x }

instance HasCompany CustomerAddress where
  theCompanyMaybe = company       
  setCompany x y = y { company = Just x }

instance HasFirstName CustomerAddress where
  theFirstNameMaybe = first_name    
  setFirstName x y = y { first_name = Just x }

instance HasLastName CustomerAddress where
  theLastNameMaybe = last_name     
  setLastName x y = y { last_name = Just x }

instance HasPhone CustomerAddress where
  thePhoneMaybe = phone         
  setPhone x y = y { phone = Just x }

instance HasProvinceName CustomerAddress where
  theProvinceNameMaybe = province      
  setProvinceName x y = y { province = Just x }

instance HasCountryText CustomerAddress where
  theCountryTextMaybe = country       
  setCountryText x y = y { country = Just x }

instance HasZip CustomerAddress where
  theZipMaybe = zip           
  setZip x y = y { zip = Just x }

instance HasName CustomerAddress where
  theNameMaybe = name          
  setName x y = y { name = Just x }

instance HasProvinceCode CustomerAddress where
  theProvinceCodeMaybe = province_code 
  setProvinceCode x y = y { province_code = Just x }

instance HasCountryCode CustomerAddress where
  theCountryCodeMaybe = country_code  
  setCountryCode x y = y { country_code = Just x }

instance HasCountryName CustomerAddress where
  theCountryNameMaybe = country_name  
  setCountryName x y = y { country_name = Just x }

