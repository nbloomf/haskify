{-# LANGUAGE OverloadedStrings #-}

module Haskify.Value.ShipmentStatus (ShipmentStatus(..), HasShipmentStatus, theShipmentStatusMaybe, theShipmentStatus, setShipmentStatus) where

import Data.Text (Text)
import Data.Aeson
import Haskify.HaskifyM

data ShipmentStatus
 = ShipmentStatusConfirmed
 | ShipmentStatusInTransit
 | ShipmentStatusOutForDelivery
 | ShipmentStatusDelivered
 | ShipmentStatusFailure
 deriving Eq

class HasShipmentStatus t where
  theShipmentStatusMaybe :: t -> Maybe ShipmentStatus
  setShipmentStatus :: ShipmentStatus -> t -> t

  theShipmentStatus :: t -> HaskifyM ShipmentStatus
  theShipmentStatus x = case theShipmentStatusMaybe x of
    Nothing -> fieldDNE "ShipmentStatus"
    Just y -> return y

instance Show ShipmentStatus where
  show ShipmentStatusConfirmed = "confirmed"
  show ShipmentStatusInTransit = "in_transit"
  show ShipmentStatusOutForDelivery = "out_for_delivery"
  show ShipmentStatusDelivered = "delivered"
  show ShipmentStatusFailure = "failure"

instance FromJSON ShipmentStatus where
  parseJSON = withText "ShipmentStatus" $ \s -> case s of
    "confirmed" -> return ShipmentStatusConfirmed
    "in_transit" -> return ShipmentStatusInTransit
    "out_for_delivery" -> return ShipmentStatusOutForDelivery
    "delivered" -> return ShipmentStatusDelivered
    "failure" -> return ShipmentStatusFailure
    _ -> fail "expecting: confirmed in_transit out_for_delivery delivered failure "

instance ToJSON ShipmentStatus where
  toJSON ShipmentStatusConfirmed = String "confirmed"
  toJSON ShipmentStatusInTransit = String "in_transit"
  toJSON ShipmentStatusOutForDelivery = String "out_for_delivery"
  toJSON ShipmentStatusDelivered = String "delivered"
  toJSON ShipmentStatusFailure = String "failure"

