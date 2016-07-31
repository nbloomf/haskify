{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Haskify.Opt.GetBlogsIDArticlesOpt (GetBlogsIDArticlesOpt()) where

import Data.Text (Text, intercalate, pack, concat)
import Data.Aeson
import Data.Time.Clock (UTCTime)

import Haskify.Types

import Haskify.Value.PublishedStatus

data GetBlogsIDArticlesOpt = GetBlogsIDArticlesOpt
 { limit            :: Maybe Int
 , page             :: Maybe Int
 , since_id         :: Maybe IDNumber
 , created_at_min   :: Maybe DateTime
 , created_at_max   :: Maybe DateTime
 , updated_at_min   :: Maybe DateTime
 , updated_at_max   :: Maybe DateTime
 , published_at_min :: Maybe DateTime
 , published_at_max :: Maybe DateTime
 , published_status :: Maybe PublishedStatus
 , fields           :: Maybe Text
 } deriving Show

instance Opt GetBlogsIDArticlesOpt where
  emptyOpt = GetBlogsIDArticlesOpt
    { limit            = Nothing
    , page             = Nothing
    , since_id         = Nothing
    , created_at_min   = Nothing
    , created_at_max   = Nothing
    , updated_at_min   = Nothing
    , updated_at_max   = Nothing
    , published_at_min = Nothing
    , published_at_max = Nothing
    , published_status = Nothing
    , fields           = Nothing
    }

instance ToURLArgs GetBlogsIDArticlesOpt where
  toURLArgs GetBlogsIDArticlesOpt{..} = intercalate "," $ filter (/= "")
    [ case limit of
        Nothing -> ""
        Just x  -> Data.Text.concat ["limit=", pack $ show x]
    , case page of
        Nothing -> ""
        Just x  -> Data.Text.concat ["page=", pack $ show x]
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

instance HasOptCreatedAtMin GetBlogsIDArticlesOpt where
  optCreatedAtMin x y = y { created_at_min = Just x }

instance HasOptCreatedAtMax GetBlogsIDArticlesOpt where
  optCreatedAtMax x y = y { created_at_max = Just x }

instance HasOptLimit GetBlogsIDArticlesOpt where
  optLimit x y = y { limit = Just x }

instance HasOptPage GetBlogsIDArticlesOpt where
  optPage x y = y { page = Just x }

instance HasOptSinceID GetBlogsIDArticlesOpt where
  optSinceID x y = y { since_id = Just x }

instance HasOptUpdatedAtMin GetBlogsIDArticlesOpt where
  optUpdatedAtMin x y = y { updated_at_min = Just x }

instance HasOptUpdatedAtMax GetBlogsIDArticlesOpt where
  optUpdatedAtMax x y = y { updated_at_max = Just x }

