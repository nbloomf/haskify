{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Haskify.Data.UsageCharge (
  UsageCharge()
  , HasUsageCharge, theUsageChargeMaybe, setUsageCharge, theUsageCharge, HasUsageCharges, theUsageChargesMaybe, setUsageCharges, theUsageCharges
) where

import Prelude hiding (id, zip)

import Data.Text (Text, pack, concat, intercalate)
import Data.Aeson
import Data.Time.Clock (UTCTime)

import Haskify.Types
import Haskify.DataFields
import Haskify.JSON
import Haskify.HaskifyM

data UsageCharge = UsageCharge
 { created_at                      :: Maybe DateTime
 , description                     :: Maybe Text
 , id                              :: Maybe IDNumber
 , price                           :: Maybe Text
 , recurring_application_charge_id :: Maybe IDNumber
 , updated_at                      :: Maybe DateTime
 } deriving (Eq, Show)

class HasUsageCharge t where
  theUsageChargeMaybe :: t -> Maybe UsageCharge
  setUsageCharge :: UsageCharge -> t -> t

  theUsageCharge :: t -> HaskifyM UsageCharge
  theUsageCharge x = case theUsageChargeMaybe x of
    Nothing -> fieldDNE "UsageCharge"
    Just y -> return y

class HasUsageCharges t where
  theUsageChargesMaybe :: t -> Maybe [UsageCharge]
  setUsageCharges :: [UsageCharge] -> t -> t

  theUsageCharges :: t -> HaskifyM [UsageCharge]
  theUsageCharges x = case theUsageChargesMaybe x of
    Nothing -> fieldDNE "UsageCharge"
    Just y -> return y

instance NullObject UsageCharge where
  nullObject = UsageCharge
    { created_at                      = Nothing
    , description                     = Nothing
    , id                              = Nothing
    , price                           = Nothing
    , recurring_application_charge_id = Nothing
    , updated_at                      = Nothing
   }

instance FromJSON UsageCharge where
  parseJSON = withObject "UsageCharge" $ \o -> do
    created_at                      <- o .:? "created_at"
    description                     <- o .:? "description"
    id                              <- o .:? "id"
    price                           <- o .:? "price"
    recurring_application_charge_id <- o .:? "recurring_application_charge_id"
    updated_at                      <- o .:? "updated_at"
    return UsageCharge{..}


instance ToJSON UsageCharge where
  toJSON UsageCharge{..} = objectSansNull
    [ "created_at"                      .= created_at
    , "description"                     .= description
    , "id"                              .= id
    , "price"                           .= price
    , "recurring_application_charge_id" .= recurring_application_charge_id
    , "updated_at"                      .= updated_at
    ]


instance ToURLArgs UsageCharge where
  toURLArgs UsageCharge{..} = intercalate "&" $ filter (/= "")
    [ case created_at of
        Nothing -> ""
        Just x  -> Data.Text.concat ["usage_charge[created_at]=", pack $ show x]
    , case description of
        Nothing -> ""
        Just x  -> Data.Text.concat ["usage_charge[description]=", pack $ show x]
    , case id of
        Nothing -> ""
        Just x  -> Data.Text.concat ["usage_charge[id]=", pack $ show x]
    , case price of
        Nothing -> ""
        Just x  -> Data.Text.concat ["usage_charge[price]=", pack $ show x]
    , case recurring_application_charge_id of
        Nothing -> ""
        Just x  -> Data.Text.concat ["usage_charge[recurring_application_charge_id]=", pack $ show x]
    , case updated_at of
        Nothing -> ""
        Just x  -> Data.Text.concat ["usage_charge[updated_at]=", pack $ show x]
    ]

instance HasCreatedAt UsageCharge where
  theCreatedAtMaybe = created_at                      
  setCreatedAt x y = y { created_at = Just x }

instance HasDescription UsageCharge where
  theDescriptionMaybe = description                     
  setDescription x y = y { description = Just x }

instance HasID UsageCharge where
  theIDMaybe = id                              
  setID x y = y { id = Just x }

instance HasPrice UsageCharge where
  thePriceMaybe = price                           
  setPrice x y = y { price = Just x }

instance HasRecurringApplicationChargeID UsageCharge where
  theRecurringApplicationChargeIDMaybe = recurring_application_charge_id 
  setRecurringApplicationChargeID x y = y { recurring_application_charge_id = Just x }

instance HasUpdatedAt UsageCharge where
  theUpdatedAtMaybe = updated_at                      
  setUpdatedAt x y = y { updated_at = Just x }

