{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Haskify.Opt.GetBlogsOpt (GetBlogsOpt()) where

import Data.Text (Text, intercalate, pack, concat)
import Data.Aeson
import Data.Time.Clock (UTCTime)

import Haskify.Types


data GetBlogsOpt = GetBlogsOpt
 { since_id :: Maybe IDNumber
 , handle   :: Maybe Text
 , fields   :: Maybe Text
 } deriving Show

instance Opt GetBlogsOpt where
  emptyOpt = GetBlogsOpt
    { since_id = Nothing
    , handle   = Nothing
    , fields   = Nothing
    }

instance ToURLArgs GetBlogsOpt where
  toURLArgs GetBlogsOpt{..} = intercalate "," $ filter (/= "")
    [ case since_id of
        Nothing -> ""
        Just x  -> Data.Text.concat ["since_id=", pack $ show x]
    , case handle of
        Nothing -> ""
        Just x  -> Data.Text.concat ["handle=", pack $ show x]
    , case fields of
        Nothing -> ""
        Just x  -> Data.Text.concat ["fields=", pack $ show x]
    ]

instance HasOptSinceID GetBlogsOpt where
  optSinceID x y = y { since_id = Just x }

