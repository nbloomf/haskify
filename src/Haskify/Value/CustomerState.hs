{-# LANGUAGE OverloadedStrings #-}

module Haskify.Value.CustomerState (CustomerState(..), HasCustomerState, theCustomerStateMaybe, theCustomerState, setCustomerState) where

import Data.Text (Text)
import Data.Aeson
import Haskify.HaskifyM

data CustomerState
 = CustomerStateDisabled
 | CustomerStateDecline
 | CustomerStateInvited
 deriving Eq

class HasCustomerState t where
  theCustomerStateMaybe :: t -> Maybe CustomerState
  setCustomerState :: CustomerState -> t -> t

  theCustomerState :: t -> HaskifyM CustomerState
  theCustomerState x = case theCustomerStateMaybe x of
    Nothing -> fieldDNE "CustomerState"
    Just y -> return y

instance Show CustomerState where
  show CustomerStateDisabled = "disabled"
  show CustomerStateDecline = "decline"
  show CustomerStateInvited = "invited"

instance FromJSON CustomerState where
  parseJSON = withText "CustomerState" $ \s -> case s of
    "disabled" -> return CustomerStateDisabled
    "decline" -> return CustomerStateDecline
    "invited" -> return CustomerStateInvited
    _ -> fail "expecting: disabled decline invited "

instance ToJSON CustomerState where
  toJSON CustomerStateDisabled = String "disabled"
  toJSON CustomerStateDecline = String "decline"
  toJSON CustomerStateInvited = String "invited"

