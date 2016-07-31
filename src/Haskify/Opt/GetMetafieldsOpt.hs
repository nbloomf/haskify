{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Haskify.Opt.GetMetafieldsOpt (GetMetafieldsOpt()) where

import Data.Text (Text, intercalate, pack, concat)
import Data.Aeson
import Data.Time.Clock (UTCTime)

import Haskify.Types

import Haskify.Value.ValueType

data GetMetafieldsOpt = GetMetafieldsOpt
 { limit          :: Maybe Int
 , since_id       :: Maybe IDNumber
 , created_at_min :: Maybe DateTime
 , created_at_max :: Maybe DateTime
 , updated_at_min :: Maybe DateTime
 , updated_at_max :: Maybe DateTime
 , namespace      :: Maybe Text
 , key            :: Maybe Text
 , value_type     :: Maybe ValueType
 , fields         :: Maybe Text
 } deriving Show

instance Opt GetMetafieldsOpt where
  emptyOpt = GetMetafieldsOpt
    { limit          = Nothing
    , since_id       = Nothing
    , created_at_min = Nothing
    , created_at_max = Nothing
    , updated_at_min = Nothing
    , updated_at_max = Nothing
    , namespace      = Nothing
    , key            = Nothing
    , value_type     = Nothing
    , fields         = Nothing
    }

instance ToURLArgs GetMetafieldsOpt where
  toURLArgs GetMetafieldsOpt{..} = intercalate "," $ filter (/= "")
    [ case limit of
        Nothing -> ""
        Just x  -> Data.Text.concat ["limit=", pack $ show x]
    , case since_id of
        Nothing -> ""
        Just x  -> Data.Text.concat ["since_id=", pack $ show x]
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
    , case namespace of
        Nothing -> ""
        Just x  -> Data.Text.concat ["namespace=", pack $ show x]
    , case key of
        Nothing -> ""
        Just x  -> Data.Text.concat ["key=", pack $ show x]
    , case value_type of
        Nothing -> ""
        Just x  -> Data.Text.concat ["value_type=", pack $ show x]
    , case fields of
        Nothing -> ""
        Just x  -> Data.Text.concat ["fields=", pack $ show x]
    ]

instance HasOptCreatedAtMin GetMetafieldsOpt where
  optCreatedAtMin x y = y { created_at_min = Just x }

instance HasOptCreatedAtMax GetMetafieldsOpt where
  optCreatedAtMax x y = y { created_at_max = Just x }

instance HasOptLimit GetMetafieldsOpt where
  optLimit x y = y { limit = Just x }

instance HasOptSinceID GetMetafieldsOpt where
  optSinceID x y = y { since_id = Just x }

instance HasOptUpdatedAtMin GetMetafieldsOpt where
  optUpdatedAtMin x y = y { updated_at_min = Just x }

instance HasOptUpdatedAtMax GetMetafieldsOpt where
  optUpdatedAtMax x y = y { updated_at_max = Just x }

