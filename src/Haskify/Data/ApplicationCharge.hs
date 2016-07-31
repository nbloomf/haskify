{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Haskify.Data.ApplicationCharge (
  ApplicationCharge()
  , HasApplicationCharge, theApplicationChargeMaybe, setApplicationCharge, theApplicationCharge, HasApplicationCharges, theApplicationChargesMaybe, setApplicationCharges, theApplicationCharges
) where

import Prelude hiding (id, zip)

import Data.Text (Text, pack, concat, intercalate)
import Data.Aeson
import Data.Time.Clock (UTCTime)

import Haskify.Types
import Haskify.DataFields
import Haskify.JSON
import Haskify.HaskifyM
import Haskify.Value.ApplicationChargeStatus
import Haskify.Value.ApplicationChargeTest

data ApplicationCharge = ApplicationCharge
 { confirmation_url :: Maybe Text
 , created_at       :: Maybe DateTime
 , id               :: Maybe IDNumber
 , name             :: Maybe Text
 , price            :: Maybe Text
 , return_url       :: Maybe Text
 , status           :: Maybe ApplicationChargeStatus
 , test             :: Maybe ApplicationChargeTest
 , updated_at       :: Maybe DateTime
 } deriving (Eq, Show)

class HasApplicationCharge t where
  theApplicationChargeMaybe :: t -> Maybe ApplicationCharge
  setApplicationCharge :: ApplicationCharge -> t -> t

  theApplicationCharge :: t -> HaskifyM ApplicationCharge
  theApplicationCharge x = case theApplicationChargeMaybe x of
    Nothing -> fieldDNE "ApplicationCharge"
    Just y -> return y

class HasApplicationCharges t where
  theApplicationChargesMaybe :: t -> Maybe [ApplicationCharge]
  setApplicationCharges :: [ApplicationCharge] -> t -> t

  theApplicationCharges :: t -> HaskifyM [ApplicationCharge]
  theApplicationCharges x = case theApplicationChargesMaybe x of
    Nothing -> fieldDNE "ApplicationCharge"
    Just y -> return y

instance NullObject ApplicationCharge where
  nullObject = ApplicationCharge
    { confirmation_url = Nothing
    , created_at       = Nothing
    , id               = Nothing
    , name             = Nothing
    , price            = Nothing
    , return_url       = Nothing
    , status           = Nothing
    , test             = Nothing
    , updated_at       = Nothing
   }

instance FromJSON ApplicationCharge where
  parseJSON = withObject "ApplicationCharge" $ \o -> do
    confirmation_url <- o .:? "confirmation_url"
    created_at       <- o .:? "created_at"
    id               <- o .:? "id"
    name             <- o .:? "name"
    price            <- o .:? "price"
    return_url       <- o .:? "return_url"
    status           <- o .:? "status"
    test             <- o .:? "test"
    updated_at       <- o .:? "updated_at"
    return ApplicationCharge{..}


instance ToJSON ApplicationCharge where
  toJSON ApplicationCharge{..} = objectSansNull
    [ "confirmation_url" .= confirmation_url
    , "created_at"       .= created_at
    , "id"               .= id
    , "name"             .= name
    , "price"            .= price
    , "return_url"       .= return_url
    , "status"           .= status
    , "test"             .= test
    , "updated_at"       .= updated_at
    ]


instance ToURLArgs ApplicationCharge where
  toURLArgs ApplicationCharge{..} = intercalate "&" $ filter (/= "")
    [ case confirmation_url of
        Nothing -> ""
        Just x  -> Data.Text.concat ["application_charge[confirmation_url]=", pack $ show x]
    , case created_at of
        Nothing -> ""
        Just x  -> Data.Text.concat ["application_charge[created_at]=", pack $ show x]
    , case id of
        Nothing -> ""
        Just x  -> Data.Text.concat ["application_charge[id]=", pack $ show x]
    , case name of
        Nothing -> ""
        Just x  -> Data.Text.concat ["application_charge[name]=", pack $ show x]
    , case price of
        Nothing -> ""
        Just x  -> Data.Text.concat ["application_charge[price]=", pack $ show x]
    , case return_url of
        Nothing -> ""
        Just x  -> Data.Text.concat ["application_charge[return_url]=", pack $ show x]
    , case status of
        Nothing -> ""
        Just x  -> Data.Text.concat ["application_charge[status]=", pack $ show x]
    , case test of
        Nothing -> ""
        Just x  -> Data.Text.concat ["application_charge[test]=", pack $ show x]
    , case updated_at of
        Nothing -> ""
        Just x  -> Data.Text.concat ["application_charge[updated_at]=", pack $ show x]
    ]

instance HasConfirmationURL ApplicationCharge where
  theConfirmationURLMaybe = confirmation_url 
  setConfirmationURL x y = y { confirmation_url = Just x }

instance HasCreatedAt ApplicationCharge where
  theCreatedAtMaybe = created_at       
  setCreatedAt x y = y { created_at = Just x }

instance HasID ApplicationCharge where
  theIDMaybe = id               
  setID x y = y { id = Just x }

instance HasName ApplicationCharge where
  theNameMaybe = name             
  setName x y = y { name = Just x }

instance HasPrice ApplicationCharge where
  thePriceMaybe = price            
  setPrice x y = y { price = Just x }

instance HasReturnURL ApplicationCharge where
  theReturnURLMaybe = return_url       
  setReturnURL x y = y { return_url = Just x }

instance HasApplicationChargeStatus ApplicationCharge where
  theApplicationChargeStatusMaybe = status           
  setApplicationChargeStatus x y = y { status = Just x }

instance HasApplicationChargeTest ApplicationCharge where
  theApplicationChargeTestMaybe = test             
  setApplicationChargeTest x y = y { test = Just x }

instance HasUpdatedAt ApplicationCharge where
  theUpdatedAtMaybe = updated_at       
  setUpdatedAt x y = y { updated_at = Just x }

