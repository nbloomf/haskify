{-# LANGUAGE OverloadedStrings #-}

module Haskify.Value.PublishedStatus (PublishedStatus(..), HasPublishedStatus, thePublishedStatusMaybe, thePublishedStatus, setPublishedStatus) where

import Data.Text (Text)
import Data.Aeson
import Haskify.HaskifyM

data PublishedStatus
 = PublishedStatusPublished
 | PublishedStatusUnpublished
 | PublishedStatusAny
 deriving Eq

class HasPublishedStatus t where
  thePublishedStatusMaybe :: t -> Maybe PublishedStatus
  setPublishedStatus :: PublishedStatus -> t -> t

  thePublishedStatus :: t -> HaskifyM PublishedStatus
  thePublishedStatus x = case thePublishedStatusMaybe x of
    Nothing -> fieldDNE "PublishedStatus"
    Just y -> return y

instance Show PublishedStatus where
  show PublishedStatusPublished = "published"
  show PublishedStatusUnpublished = "unpublished"
  show PublishedStatusAny = "any"

instance FromJSON PublishedStatus where
  parseJSON = withText "PublishedStatus" $ \s -> case s of
    "published" -> return PublishedStatusPublished
    "unpublished" -> return PublishedStatusUnpublished
    "any" -> return PublishedStatusAny
    _ -> fail "expecting: published unpublished any "

instance ToJSON PublishedStatus where
  toJSON PublishedStatusPublished = String "published"
  toJSON PublishedStatusUnpublished = String "unpublished"
  toJSON PublishedStatusAny = String "any"

