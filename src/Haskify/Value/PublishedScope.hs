{-# LANGUAGE OverloadedStrings #-}

module Haskify.Value.PublishedScope (PublishedScope(..), HasPublishedScope, thePublishedScopeMaybe, thePublishedScope, setPublishedScope) where

import Data.Text (Text)
import Data.Aeson
import Haskify.HaskifyM

data PublishedScope
 = PublishedScopeGlobal
 | PublishedScopeWeb
 deriving Eq

class HasPublishedScope t where
  thePublishedScopeMaybe :: t -> Maybe PublishedScope
  setPublishedScope :: PublishedScope -> t -> t

  thePublishedScope :: t -> HaskifyM PublishedScope
  thePublishedScope x = case thePublishedScopeMaybe x of
    Nothing -> fieldDNE "PublishedScope"
    Just y -> return y

instance Show PublishedScope where
  show PublishedScopeGlobal = "global"
  show PublishedScopeWeb = "web"

instance FromJSON PublishedScope where
  parseJSON = withText "PublishedScope" $ \s -> case s of
    "global" -> return PublishedScopeGlobal
    "web" -> return PublishedScopeWeb
    _ -> fail "expecting: global web "

instance ToJSON PublishedScope where
  toJSON PublishedScopeGlobal = String "global"
  toJSON PublishedScopeWeb = String "web"

