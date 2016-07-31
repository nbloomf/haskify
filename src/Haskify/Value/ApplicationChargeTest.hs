{-# LANGUAGE OverloadedStrings #-}

module Haskify.Value.ApplicationChargeTest (ApplicationChargeTest(..), HasApplicationChargeTest, theApplicationChargeTestMaybe, theApplicationChargeTest, setApplicationChargeTest) where

import Data.Text (Text)
import Data.Aeson
import Haskify.HaskifyM

data ApplicationChargeTest
 = ApplicationChargeTestNull
 | ApplicationChargeTestTrue
 deriving Eq

class HasApplicationChargeTest t where
  theApplicationChargeTestMaybe :: t -> Maybe ApplicationChargeTest
  setApplicationChargeTest :: ApplicationChargeTest -> t -> t

  theApplicationChargeTest :: t -> HaskifyM ApplicationChargeTest
  theApplicationChargeTest x = case theApplicationChargeTestMaybe x of
    Nothing -> fieldDNE "ApplicationChargeTest"
    Just y -> return y

instance Show ApplicationChargeTest where
  show ApplicationChargeTestNull = "null"
  show ApplicationChargeTestTrue = "true"

instance FromJSON ApplicationChargeTest where
  parseJSON = withText "ApplicationChargeTest" $ \s -> case s of
    "null" -> return ApplicationChargeTestNull
    "true" -> return ApplicationChargeTestTrue
    _ -> fail "expecting: null true "

instance ToJSON ApplicationChargeTest where
  toJSON ApplicationChargeTestNull = String "null"
  toJSON ApplicationChargeTestTrue = String "true"

