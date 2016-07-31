{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Haskify.Data.Theme (
  Theme()
  , HasTheme, theThemeMaybe, setTheme, theTheme, HasThemes, theThemesMaybe, setThemes, theThemes
) where

import Prelude hiding (id, zip)

import Data.Text (Text, pack, concat, intercalate)
import Data.Aeson
import Data.Time.Clock (UTCTime)

import Haskify.Types
import Haskify.DataFields
import Haskify.JSON
import Haskify.HaskifyM
import Haskify.Value.ThemeRole

data Theme = Theme
 { created_at  :: Maybe DateTime
 , id          :: Maybe IDNumber
 , name        :: Maybe Text
 , role        :: Maybe ThemeRole
 , updated_at  :: Maybe DateTime
 , previewable :: Maybe Bool
 , processing  :: Maybe Bool
 } deriving (Eq, Show)

class HasTheme t where
  theThemeMaybe :: t -> Maybe Theme
  setTheme :: Theme -> t -> t

  theTheme :: t -> HaskifyM Theme
  theTheme x = case theThemeMaybe x of
    Nothing -> fieldDNE "Theme"
    Just y -> return y

class HasThemes t where
  theThemesMaybe :: t -> Maybe [Theme]
  setThemes :: [Theme] -> t -> t

  theThemes :: t -> HaskifyM [Theme]
  theThemes x = case theThemesMaybe x of
    Nothing -> fieldDNE "Theme"
    Just y -> return y

instance NullObject Theme where
  nullObject = Theme
    { created_at  = Nothing
    , id          = Nothing
    , name        = Nothing
    , role        = Nothing
    , updated_at  = Nothing
    , previewable = Nothing
    , processing  = Nothing
   }

instance FromJSON Theme where
  parseJSON = withObject "Theme" $ \o -> do
    created_at  <- o .:? "created_at"
    id          <- o .:? "id"
    name        <- o .:? "name"
    role        <- o .:? "role"
    updated_at  <- o .:? "updated_at"
    previewable <- o .:? "previewable"
    processing  <- o .:? "processing"
    return Theme{..}


instance ToJSON Theme where
  toJSON Theme{..} = objectSansNull
    [ "created_at"  .= created_at
    , "id"          .= id
    , "name"        .= name
    , "role"        .= role
    , "updated_at"  .= updated_at
    , "previewable" .= previewable
    , "processing"  .= processing
    ]


instance ToURLArgs Theme where
  toURLArgs Theme{..} = intercalate "&" $ filter (/= "")
    [ case created_at of
        Nothing -> ""
        Just x  -> Data.Text.concat ["theme[created_at]=", pack $ show x]
    , case id of
        Nothing -> ""
        Just x  -> Data.Text.concat ["theme[id]=", pack $ show x]
    , case name of
        Nothing -> ""
        Just x  -> Data.Text.concat ["theme[name]=", pack $ show x]
    , case role of
        Nothing -> ""
        Just x  -> Data.Text.concat ["theme[role]=", pack $ show x]
    , case updated_at of
        Nothing -> ""
        Just x  -> Data.Text.concat ["theme[updated_at]=", pack $ show x]
    , case previewable of
        Nothing -> ""
        Just x  -> Data.Text.concat ["theme[previewable]=", pack $ show x]
    , case processing of
        Nothing -> ""
        Just x  -> Data.Text.concat ["theme[processing]=", pack $ show x]
    ]

instance HasCreatedAt Theme where
  theCreatedAtMaybe = created_at  
  setCreatedAt x y = y { created_at = Just x }

instance HasID Theme where
  theIDMaybe = id          
  setID x y = y { id = Just x }

instance HasName Theme where
  theNameMaybe = name        
  setName x y = y { name = Just x }

instance HasThemeRole Theme where
  theThemeRoleMaybe = role        
  setThemeRole x y = y { role = Just x }

instance HasUpdatedAt Theme where
  theUpdatedAtMaybe = updated_at  
  setUpdatedAt x y = y { updated_at = Just x }

instance HasPreviewable Theme where
  thePreviewableMaybe = previewable 
  setPreviewable x y = y { previewable = Just x }

instance HasProcessing Theme where
  theProcessingMaybe = processing  
  setProcessing x y = y { processing = Just x }

