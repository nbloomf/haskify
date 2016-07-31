{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Haskify.Data.Redirect (
  Redirect()
  , HasRedirect, theRedirectMaybe, setRedirect, theRedirect, HasRedirects, theRedirectsMaybe, setRedirects, theRedirects
) where

import Prelude hiding (id, zip)

import Data.Text (Text, pack, concat, intercalate)
import Data.Aeson
import Data.Time.Clock (UTCTime)

import Haskify.Types
import Haskify.DataFields
import Haskify.JSON
import Haskify.HaskifyM

data Redirect = Redirect
 { id     :: Maybe IDNumber
 , path   :: Maybe Text
 , target :: Maybe Text
 } deriving (Eq, Show)

class HasRedirect t where
  theRedirectMaybe :: t -> Maybe Redirect
  setRedirect :: Redirect -> t -> t

  theRedirect :: t -> HaskifyM Redirect
  theRedirect x = case theRedirectMaybe x of
    Nothing -> fieldDNE "Redirect"
    Just y -> return y

class HasRedirects t where
  theRedirectsMaybe :: t -> Maybe [Redirect]
  setRedirects :: [Redirect] -> t -> t

  theRedirects :: t -> HaskifyM [Redirect]
  theRedirects x = case theRedirectsMaybe x of
    Nothing -> fieldDNE "Redirect"
    Just y -> return y

instance NullObject Redirect where
  nullObject = Redirect
    { id     = Nothing
    , path   = Nothing
    , target = Nothing
   }

instance FromJSON Redirect where
  parseJSON = withObject "Redirect" $ \o -> do
    id     <- o .:? "id"
    path   <- o .:? "path"
    target <- o .:? "target"
    return Redirect{..}


instance ToJSON Redirect where
  toJSON Redirect{..} = objectSansNull
    [ "id"     .= id
    , "path"   .= path
    , "target" .= target
    ]


instance ToURLArgs Redirect where
  toURLArgs Redirect{..} = intercalate "&" $ filter (/= "")
    [ case id of
        Nothing -> ""
        Just x  -> Data.Text.concat ["redirect[id]=", pack $ show x]
    , case path of
        Nothing -> ""
        Just x  -> Data.Text.concat ["redirect[path]=", pack $ show x]
    , case target of
        Nothing -> ""
        Just x  -> Data.Text.concat ["redirect[target]=", pack $ show x]
    ]

instance HasID Redirect where
  theIDMaybe = id     
  setID x y = y { id = Just x }

instance HasPath Redirect where
  thePathMaybe = path   
  setPath x y = y { path = Just x }

instance HasTarget Redirect where
  theTargetMaybe = target 
  setTarget x y = y { target = Just x }

