{-# LANGUAGE OverloadedStrings #-}

module Haskify.Value.InventoryPolicy (InventoryPolicy(..), HasInventoryPolicy, theInventoryPolicyMaybe, theInventoryPolicy, setInventoryPolicy) where

import Data.Text (Text)
import Data.Aeson
import Haskify.HaskifyM

data InventoryPolicy
 = InventoryPolicyDeny
 | InventoryPolicyContinue
 deriving Eq

class HasInventoryPolicy t where
  theInventoryPolicyMaybe :: t -> Maybe InventoryPolicy
  setInventoryPolicy :: InventoryPolicy -> t -> t

  theInventoryPolicy :: t -> HaskifyM InventoryPolicy
  theInventoryPolicy x = case theInventoryPolicyMaybe x of
    Nothing -> fieldDNE "InventoryPolicy"
    Just y -> return y

instance Show InventoryPolicy where
  show InventoryPolicyDeny = "deny"
  show InventoryPolicyContinue = "continue"

instance FromJSON InventoryPolicy where
  parseJSON = withText "InventoryPolicy" $ \s -> case s of
    "deny" -> return InventoryPolicyDeny
    "continue" -> return InventoryPolicyContinue
    _ -> fail "expecting: deny continue "

instance ToJSON InventoryPolicy where
  toJSON InventoryPolicyDeny = String "deny"
  toJSON InventoryPolicyContinue = String "continue"

