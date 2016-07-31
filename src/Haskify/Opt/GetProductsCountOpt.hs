{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Haskify.Opt.GetProductsCountOpt (GetProductsCountOpt()) where

import Data.Text (Text, intercalate, pack, concat)
import Data.Aeson
import Data.Time.Clock (UTCTime)

import Haskify.Types

import Haskify.Value.PublishedStatus

data GetProductsCountOpt = GetProductsCountOpt
 { vendor           :: Maybe Text
 , product_type     :: Maybe Text
 , collection_id    :: Maybe IDNumber
 , created_at_min   :: Maybe DateTime
 , created_at_max   :: Maybe DateTime
 , updated_at_min   :: Maybe DateTime
 , updated_at_max   :: Maybe DateTime
 , published_at_min :: Maybe DateTime
 , published_at_max :: Maybe DateTime
 , published_status :: Maybe PublishedStatus
 } deriving Show

instance Opt GetProductsCountOpt where
  emptyOpt = GetProductsCountOpt
    { vendor           = Nothing
    , product_type     = Nothing
    , collection_id    = Nothing
    , created_at_min   = Nothing
    , created_at_max   = Nothing
    , updated_at_min   = Nothing
    , updated_at_max   = Nothing
    , published_at_min = Nothing
    , published_at_max = Nothing
    , published_status = Nothing
    }

instance ToURLArgs GetProductsCountOpt where
  toURLArgs GetProductsCountOpt{..} = intercalate "," $ filter (/= "")
    [ case vendor of
        Nothing -> ""
        Just x  -> Data.Text.concat ["vendor=", pack $ show x]
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
    ]

instance HasOptCreatedAtMin GetProductsCountOpt where
  optCreatedAtMin x y = y { created_at_min = Just x }

instance HasOptCreatedAtMax GetProductsCountOpt where
  optCreatedAtMax x y = y { created_at_max = Just x }

instance HasOptUpdatedAtMin GetProductsCountOpt where
  optUpdatedAtMin x y = y { updated_at_min = Just x }

instance HasOptUpdatedAtMax GetProductsCountOpt where
  optUpdatedAtMax x y = y { updated_at_max = Just x }

instance HasOptVendor GetProductsCountOpt where
  optVendor x y = y { vendor = Just x }

