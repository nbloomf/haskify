{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Haskify.Opt.GetBlogsIDArticlesCountOpt (GetBlogsIDArticlesCountOpt()) where

import Data.Text (Text, intercalate, pack, concat)
import Data.Aeson
import Data.Time.Clock (UTCTime)

import Haskify.Types

import Haskify.Value.PublishedStatus

data GetBlogsIDArticlesCountOpt = GetBlogsIDArticlesCountOpt
 { created_at_min   :: Maybe DateTime
 , created_at_max   :: Maybe DateTime
 , updated_at_min   :: Maybe DateTime
 , updated_at_max   :: Maybe DateTime
 , published_at_min :: Maybe DateTime
 , published_at_max :: Maybe DateTime
 , published_status :: Maybe PublishedStatus
 } deriving Show

instance Opt GetBlogsIDArticlesCountOpt where
  emptyOpt = GetBlogsIDArticlesCountOpt
    { created_at_min   = Nothing
    , created_at_max   = Nothing
    , updated_at_min   = Nothing
    , updated_at_max   = Nothing
    , published_at_min = Nothing
    , published_at_max = Nothing
    , published_status = Nothing
    }

instance ToURLArgs GetBlogsIDArticlesCountOpt where
  toURLArgs GetBlogsIDArticlesCountOpt{..} = intercalate "," $ filter (/= "")
    [ case created_at_min of
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

instance HasOptCreatedAtMin GetBlogsIDArticlesCountOpt where
  optCreatedAtMin x y = y { created_at_min = Just x }

instance HasOptCreatedAtMax GetBlogsIDArticlesCountOpt where
  optCreatedAtMax x y = y { created_at_max = Just x }

instance HasOptUpdatedAtMin GetBlogsIDArticlesCountOpt where
  optUpdatedAtMin x y = y { updated_at_min = Just x }

instance HasOptUpdatedAtMax GetBlogsIDArticlesCountOpt where
  optUpdatedAtMax x y = y { updated_at_max = Just x }

