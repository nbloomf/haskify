{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Haskify.Opt.GetProductsOpt (GetProductsOpt()) where

import Data.Text (Text, intercalate, pack, concat)
import Data.Aeson
import Data.Time.Clock (UTCTime)

import Haskify.Types

import Haskify.Value.PublishedStatus

data GetProductsOpt = GetProductsOpt
 { ids              :: Maybe Text
 , limit            :: Maybe Int
 , page             :: Maybe Int
 , since_id         :: Maybe IDNumber
 , title            :: Maybe Text
 , vendor           :: Maybe Text
 , handle           :: Maybe Text
 , product_type     :: Maybe Text
 , collection_id    :: Maybe IDNumber
 , created_at_min   :: Maybe DateTime
 , created_at_max   :: Maybe DateTime
 , updated_at_min   :: Maybe DateTime
 , updated_at_max   :: Maybe DateTime
 , published_at_min :: Maybe DateTime
 , published_at_max :: Maybe DateTime
 , published_status :: Maybe PublishedStatus
 , fields           :: Maybe Text
 } deriving Show

instance Opt GetProductsOpt where
  emptyOpt = GetProductsOpt
    { ids              = Nothing
    , limit            = Nothing
    , page             = Nothing
    , since_id         = Nothing
    , title            = Nothing
    , vendor           = Nothing
    , handle           = Nothing
    , product_type     = Nothing
    , collection_id    = Nothing
    , created_at_min   = Nothing
    , created_at_max   = Nothing
    , updated_at_min   = Nothing
    , updated_at_max   = Nothing
    , published_at_min = Nothing
    , published_at_max = Nothing
    , published_status = Nothing
    , fields           = Nothing
    }

instance ToURLArgs GetProductsOpt where
  toURLArgs GetProductsOpt{..} = intercalate "," $ filter (/= "")
    [ case ids of
        Nothing -> ""
        Just x  -> Data.Text.concat ["ids=", pack $ show x]
    , case limit of
        Nothing -> ""
        Just x  -> Data.Text.concat ["limit=", pack $ show x]
    , case page of
        Nothing -> ""
        Just x  -> Data.Text.concat ["page=", pack $ show x]
    , case since_id of
        Nothing -> ""
        Just x  -> Data.Text.concat ["since_id=", pack $ show x]
    , case title of
        Nothing -> ""
        Just x  -> Data.Text.concat ["title=", pack $ show x]
    , case vendor of
        Nothing -> ""
        Just x  -> Data.Text.concat ["vendor=", pack $ show x]
    , case handle of
        Nothing -> ""
        Just x  -> Data.Text.concat ["handle=", pack $ show x]
    , case product_type of
        Nothing -> ""
        Just x  -> Data.Text.concat ["product_type=", pack $ show x]
    , case collection_id of
        Nothing -> ""
        Just x  -> Data.Text.concat ["collection_id=", pack $ show x]
    , case created_at_min of
        Nothing -> ""
        Just x  -> Data.Text.concat ["created_at_min=", pack $ show x]
    , case created_at_max of
        Nothing -> ""
        Just x  -> Data.Text.concat ["created_at_max=", pack $ show x]
    , case updated_at_min of
        Nothing -> ""
        Just x  -> Data.Text.concat ["updated_at_min=", pack $ show x]
    , case updated_at_max of
        Nothing -> ""
        Just x  -> Data.Text.concat ["updated_at_max=", pack $ show x]
    , case published_at_min of
        Nothing -> ""
        Just x  -> Data.Text.concat ["published_at_min=", pack $ show x]
    , case published_at_max of
        Nothing -> ""
        Just x  -> Data.Text.concat ["published_at_max=", pack $ show x]
    , case published_status of
        Nothing -> ""
        Just x  -> Data.Text.concat ["published_status=", pack $ show x]
    , case fields of
        Nothing -> ""
        Just x  -> Data.Text.concat ["fields=", pack $ show x]
    ]

instance HasOptCreatedAtMin GetProductsOpt where
  optCreatedAtMin x y = y { created_at_min = Just x }

instance HasOptCreatedAtMax GetProductsOpt where
  optCreatedAtMax x y = y { created_at_max = Just x }

instance HasOptLimit GetProductsOpt where
  optLimit x y = y { limit = Just x }

instance HasOptPage GetProductsOpt where
  optPage x y = y { page = Just x }

instance HasOptSinceID GetProductsOpt where
  optSinceID x y = y { since_id = Just x }

instance HasOptUpdatedAtMin GetProductsOpt where
  optUpdatedAtMin x y = y { updated_at_min = Just x }

instance HasOptUpdatedAtMax GetProductsOpt where
  optUpdatedAtMax x y = y { updated_at_max = Just x }

instance HasOptVendor GetProductsOpt where
  optVendor x y = y { vendor = Just x }

