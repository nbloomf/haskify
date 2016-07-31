{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Haskify.Opt.GetProductsIDOpt (GetProductsIDOpt()) where

import Data.Text (Text, intercalate, pack, concat)
import Data.Aeson
import Data.Time.Clock (UTCTime)

import Haskify.Types


data GetProductsIDOpt = GetProductsIDOpt
 { fields :: Maybe Text
 } deriving Show

instance Opt GetProductsIDOpt where
  emptyOpt = GetProductsIDOpt
    { fields = Nothing
    }

instance ToURLArgs GetProductsIDOpt where
  toURLArgs GetProductsIDOpt{..} = intercalate "," $ filter (/= "")
    [ case fields of
        Nothing -> ""
        Just x  -> Data.Text.concat ["fields=", pack $ show x]
    ]

