{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Haskify.Opt.GetBlogsIDArticlesTagsOpt (GetBlogsIDArticlesTagsOpt()) where

import Data.Text (Text, intercalate, pack, concat)
import Data.Aeson
import Data.Time.Clock (UTCTime)

import Haskify.Types


data GetBlogsIDArticlesTagsOpt = GetBlogsIDArticlesTagsOpt
 { limit   :: Maybe Int
 , popular :: Maybe Int
 } deriving Show

instance Opt GetBlogsIDArticlesTagsOpt where
  emptyOpt = GetBlogsIDArticlesTagsOpt
    { limit   = Nothing
    , popular = Nothing
    }

instance ToURLArgs GetBlogsIDArticlesTagsOpt where
  toURLArgs GetBlogsIDArticlesTagsOpt{..} = intercalate "," $ filter (/= "")
    [ case limit of
        Nothing -> ""
        Just x  -> Data.Text.concat ["limit=", pack $ show x]
    , case popular of
        Nothing -> ""
        Just x  -> Data.Text.concat ["popular=", pack $ show x]
    ]

instance HasOptLimit GetBlogsIDArticlesTagsOpt where
  optLimit x y = y { limit = Just x }

