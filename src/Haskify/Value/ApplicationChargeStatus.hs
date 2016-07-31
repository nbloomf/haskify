{-# LANGUAGE OverloadedStrings #-}

module Haskify.Value.ApplicationChargeStatus (ApplicationChargeStatus(..), HasApplicationChargeStatus, theApplicationChargeStatusMaybe, theApplicationChargeStatus, setApplicationChargeStatus) where

import Data.Text (Text)
import Data.Aeson
import Haskify.HaskifyM

data ApplicationChargeStatus
 = ApplicationChargeStatusPending
 | ApplicationChargeStatusAccepted
 | ApplicationChargeStatusDeclined
 | ApplicationChargeStatusExpired
 deriving Eq

class HasApplicationChargeStatus t where
  theApplicationChargeStatusMaybe :: t -> Maybe ApplicationChargeStatus
  setApplicationChargeStatus :: ApplicationChargeStatus -> t -> t

  theApplicationChargeStatus :: t -> HaskifyM ApplicationChargeStatus
  theApplicationChargeStatus x = case theApplicationChargeStatusMaybe x of
    Nothing -> fieldDNE "ApplicationChargeStatus"
    Just y -> return y

instance Show ApplicationChargeStatus where
  show ApplicationChargeStatusPending = "pending"
  show ApplicationChargeStatusAccepted = "accepted"
  show ApplicationChargeStatusDeclined = "declined"
  show ApplicationChargeStatusExpired = "expired"

instance FromJSON ApplicationChargeStatus where
  parseJSON = withText "ApplicationChargeStatus" $ \s -> case s of
    "pending" -> return ApplicationChargeStatusPending
    "accepted" -> return ApplicationChargeStatusAccepted
    "declined" -> return ApplicationChargeStatusDeclined
    "expired" -> return ApplicationChargeStatusExpired
    _ -> fail "expecting: pending accepted declined expired "

instance ToJSON ApplicationChargeStatus where
  toJSON ApplicationChargeStatusPending = String "pending"
  toJSON ApplicationChargeStatusAccepted = String "accepted"
  toJSON ApplicationChargeStatusDeclined = String "declined"
  toJSON ApplicationChargeStatusExpired = String "expired"

