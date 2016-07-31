{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Haskify.Data.ScriptTag (
  ScriptTag()
  , HasScriptTag, theScriptTagMaybe, setScriptTag, theScriptTag, HasScriptTags, theScriptTagsMaybe, setScriptTags, theScriptTags
) where

import Prelude hiding (id, zip)

import Data.Text (Text, pack, concat, intercalate)
import Data.Aeson
import Data.Time.Clock (UTCTime)

import Haskify.Types
import Haskify.DataFields
import Haskify.JSON
import Haskify.HaskifyM
import Haskify.Value.DisplayScope
import Haskify.Value.DOMEvent

data ScriptTag = ScriptTag
 { created_at    :: Maybe DateTime
 , event         :: Maybe DOMEvent
 , id            :: Maybe IDNumber
 , src           :: Maybe Text
 , display_scope :: Maybe DisplayScope
 , updated_at    :: Maybe DateTime
 } deriving (Eq, Show)

class HasScriptTag t where
  theScriptTagMaybe :: t -> Maybe ScriptTag
  setScriptTag :: ScriptTag -> t -> t

  theScriptTag :: t -> HaskifyM ScriptTag
  theScriptTag x = case theScriptTagMaybe x of
    Nothing -> fieldDNE "ScriptTag"
    Just y -> return y

class HasScriptTags t where
  theScriptTagsMaybe :: t -> Maybe [ScriptTag]
  setScriptTags :: [ScriptTag] -> t -> t

  theScriptTags :: t -> HaskifyM [ScriptTag]
  theScriptTags x = case theScriptTagsMaybe x of
    Nothing -> fieldDNE "ScriptTag"
    Just y -> return y

instance NullObject ScriptTag where
  nullObject = ScriptTag
    { created_at    = Nothing
    , event         = Nothing
    , id            = Nothing
    , src           = Nothing
    , display_scope = Nothing
    , updated_at    = Nothing
   }

instance FromJSON ScriptTag where
  parseJSON = withObject "ScriptTag" $ \o -> do
    created_at    <- o .:? "created_at"
    event         <- o .:? "event"
    id            <- o .:? "id"
    src           <- o .:? "src"
    display_scope <- o .:? "display_scope"
    updated_at    <- o .:? "updated_at"
    return ScriptTag{..}


instance ToJSON ScriptTag where
  toJSON ScriptTag{..} = objectSansNull
    [ "created_at"    .= created_at
    , "event"         .= event
    , "id"            .= id
    , "src"           .= src
    , "display_scope" .= display_scope
    , "updated_at"    .= updated_at
    ]


instance ToURLArgs ScriptTag where
  toURLArgs ScriptTag{..} = intercalate "&" $ filter (/= "")
    [ case created_at of
        Nothing -> ""
        Just x  -> Data.Text.concat ["script_tag[created_at]=", pack $ show x]
    , case event of
        Nothing -> ""
        Just x  -> Data.Text.concat ["script_tag[event]=", pack $ show x]
    , case id of
        Nothing -> ""
        Just x  -> Data.Text.concat ["script_tag[id]=", pack $ show x]
    , case src of
        Nothing -> ""
        Just x  -> Data.Text.concat ["script_tag[src]=", pack $ show x]
    , case display_scope of
        Nothing -> ""
        Just x  -> Data.Text.concat ["script_tag[display_scope]=", pack $ show x]
    , case updated_at of
        Nothing -> ""
        Just x  -> Data.Text.concat ["script_tag[updated_at]=", pack $ show x]
    ]

instance HasCreatedAt ScriptTag where
  theCreatedAtMaybe = created_at    
  setCreatedAt x y = y { created_at = Just x }

instance HasDOMEvent ScriptTag where
  theDOMEventMaybe = event         
  setDOMEvent x y = y { event = Just x }

instance HasID ScriptTag where
  theIDMaybe = id            
  setID x y = y { id = Just x }

instance HasSrc ScriptTag where
  theSrcMaybe = src           
  setSrc x y = y { src = Just x }

instance HasDisplayScope ScriptTag where
  theDisplayScopeMaybe = display_scope 
  setDisplayScope x y = y { display_scope = Just x }

instance HasUpdatedAt ScriptTag where
  theUpdatedAtMaybe = updated_at    
  setUpdatedAt x y = y { updated_at = Just x }

