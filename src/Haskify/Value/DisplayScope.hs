{-# LANGUAGE OverloadedStrings #-}

module Haskify.Value.DisplayScope (DisplayScope(..), HasDisplayScope, theDisplayScopeMaybe, theDisplayScope, setDisplayScope) where

import Data.Text (Text)
import Data.Aeson
import Haskify.HaskifyM

data DisplayScope
 = DisplayScopeOnlineStore
 | DisplayScopeOrderStatus
 | DisplayScopeAll
 deriving Eq

class HasDisplayScope t where
  theDisplayScopeMaybe :: t -> Maybe DisplayScope
  setDisplayScope :: DisplayScope -> t -> t

  theDisplayScope :: t -> HaskifyM DisplayScope
  theDisplayScope x = case theDisplayScopeMaybe x of
    Nothing -> fieldDNE "DisplayScope"
    Just y -> return y

instance Show DisplayScope where
  show DisplayScopeOnlineStore = "online_store"
  show DisplayScopeOrderStatus = "order_status"
  show DisplayScopeAll = "all"

instance FromJSON DisplayScope where
  parseJSON = withText "DisplayScope" $ \s -> case s of
    "online_store" -> return DisplayScopeOnlineStore
    "order_status" -> return DisplayScopeOrderStatus
    "all" -> return DisplayScopeAll
    _ -> fail "expecting: online_store order_status all "

instance ToJSON DisplayScope where
  toJSON DisplayScopeOnlineStore = String "online_store"
  toJSON DisplayScopeOrderStatus = String "order_status"
  toJSON DisplayScopeAll = String "all"

