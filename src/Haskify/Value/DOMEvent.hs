{-# LANGUAGE OverloadedStrings #-}

module Haskify.Value.DOMEvent (DOMEvent(..), HasDOMEvent, theDOMEventMaybe, theDOMEvent, setDOMEvent) where

import Data.Text (Text)
import Data.Aeson
import Haskify.HaskifyM

data DOMEvent
 = DOMEventOnLoad
 deriving Eq

class HasDOMEvent t where
  theDOMEventMaybe :: t -> Maybe DOMEvent
  setDOMEvent :: DOMEvent -> t -> t

  theDOMEvent :: t -> HaskifyM DOMEvent
  theDOMEvent x = case theDOMEventMaybe x of
    Nothing -> fieldDNE "DOMEvent"
    Just y -> return y

instance Show DOMEvent where
  show DOMEventOnLoad = "onload"

instance FromJSON DOMEvent where
  parseJSON = withText "DOMEvent" $ \s -> case s of
    "onload" -> return DOMEventOnLoad
    _ -> fail "expecting: onload "

instance ToJSON DOMEvent where
  toJSON DOMEventOnLoad = String "onload"

