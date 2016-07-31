{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Haskify.Opt.GetProductsIDVariantsOpt (GetProductsIDVariantsOpt()) where

import Data.Text (Text, intercalate, pack, concat)
import Data.Aeson
import Data.Time.Clock (UTCTime)

import Haskify.Types


data GetProductsIDVariantsOpt = GetProductsIDVariantsOpt
 { limit    :: Maybe Int
 , page     :: Maybe Int
 , since_id :: Maybe IDNumber
 , fields   :: Maybe Text
 } deriving Show

instance Opt GetProductsIDVariantsOpt where
  emptyOpt = GetProductsIDVariantsOpt
    { limit    = Nothing
    , page     = Nothing
    , since_id = Nothing
    , fields   = Nothing
    }

instance ToURLArgs GetProductsIDVariantsOpt where
  toURLArgs GetProductsIDVariantsOpt{..} = intercalate "," $ filter (/= "")
    [ case limit of
        Nothing -> ""
        Just x  -> Data.Text.concat ["limit=", pack $ show x]
    , case page of
        Nothing -> ""
        Just x  -> Data.Text.concat ["page=", pack $ show x]
    , case since_id of
        Nothing -> ""
        Just x  -> Data.Text.concat ["since_id=", pack $ show x]
    , case fields of
        Nothing -> ""
        Just x  -> Data.Text.concat ["fields=", pack $ show x]
    ]

instance HasOptLimit GetProductsIDVariantsOpt where
  optLimit x y = y { limit = Just x }

instance HasOptPage GetProductsIDVariantsOpt where
  optPage x y = y { page = Just x }

instance HasOptSinceID GetProductsIDVariantsOpt where
  optSinceID x y = y { since_id = Just x }

