{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Haskify.Data.CustomCollection (
  CustomCollection()
  , HasCustomCollection, theCustomCollectionMaybe, setCustomCollection, theCustomCollection, HasCustomCollections, theCustomCollectionsMaybe, setCustomCollections, theCustomCollections
) where

import Prelude hiding (id, zip)

import Data.Text (Text, pack, concat, intercalate)
import Data.Aeson
import Data.Time.Clock (UTCTime)

import Haskify.Types
import Haskify.DataFields
import Haskify.JSON
import Haskify.HaskifyM
import Haskify.Value.PublishedScope
import Haskify.Value.SortOrder
import Haskify.Data.Image

data CustomCollection = CustomCollection
 { body_html       :: Maybe Text
 , handle          :: Maybe Text
 , id              :: Maybe IDNumber
 , image           :: Maybe Image
 , published       :: Maybe Bool
 , published_at    :: Maybe DateTime
 , published_scope :: Maybe PublishedScope
 , sort_order      :: Maybe SortOrder
 , template_suffix :: Maybe Text
 , title           :: Maybe Text
 , updated_at      :: Maybe DateTime
 } deriving (Eq, Show)

class HasCustomCollection t where
  theCustomCollectionMaybe :: t -> Maybe CustomCollection
  setCustomCollection :: CustomCollection -> t -> t

  theCustomCollection :: t -> HaskifyM CustomCollection
  theCustomCollection x = case theCustomCollectionMaybe x of
    Nothing -> fieldDNE "CustomCollection"
    Just y -> return y

class HasCustomCollections t where
  theCustomCollectionsMaybe :: t -> Maybe [CustomCollection]
  setCustomCollections :: [CustomCollection] -> t -> t

  theCustomCollections :: t -> HaskifyM [CustomCollection]
  theCustomCollections x = case theCustomCollectionsMaybe x of
    Nothing -> fieldDNE "CustomCollection"
    Just y -> return y

instance NullObject CustomCollection where
  nullObject = CustomCollection
    { body_html       = Nothing
    , handle          = Nothing
    , id              = Nothing
    , image           = Nothing
    , published       = Nothing
    , published_at    = Nothing
    , published_scope = Nothing
    , sort_order      = Nothing
    , template_suffix = Nothing
    , title           = Nothing
    , updated_at      = Nothing
   }

instance FromJSON CustomCollection where
  parseJSON = withObject "CustomCollection" $ \o -> do
    body_html       <- o .:? "body_html"
    handle          <- o .:? "handle"
    id              <- o .:? "id"
    image           <- o .:? "image"
    published       <- o .:? "published"
    published_at    <- o .:? "published_at"
    published_scope <- o .:? "published_scope"
    sort_order      <- o .:? "sort_order"
    template_suffix <- o .:? "template_suffix"
    title           <- o .:? "title"
    updated_at      <- o .:? "updated_at"
    return CustomCollection{..}


instance ToJSON CustomCollection where
  toJSON CustomCollection{..} = objectSansNull
    [ "body_html"       .= body_html
    , "handle"          .= handle
    , "id"              .= id
    , "image"           .= image
    , "published"       .= published
    , "published_at"    .= published_at
    , "published_scope" .= published_scope
    , "sort_order"      .= sort_order
    , "template_suffix" .= template_suffix
    , "title"           .= title
    , "updated_at"      .= updated_at
    ]


instance ToURLArgs CustomCollection where
  toURLArgs CustomCollection{..} = intercalate "&" $ filter (/= "")
    [ case body_html of
        Nothing -> ""
        Just x  -> Data.Text.concat ["custom_collection[body_html]=", pack $ show x]
    , case handle of
        Nothing -> ""
        Just x  -> Data.Text.concat ["custom_collection[handle]=", pack $ show x]
    , case id of
        Nothing -> ""
        Just x  -> Data.Text.concat ["custom_collection[id]=", pack $ show x]
    , case image of
        Nothing -> ""
        Just x  -> Data.Text.concat ["custom_collection[image]=", pack $ show x]
    , case published of
        Nothing -> ""
        Just x  -> Data.Text.concat ["custom_collection[published]=", pack $ show x]
    , case published_at of
        Nothing -> ""
        Just x  -> Data.Text.concat ["custom_collection[published_at]=", pack $ show x]
    , case published_scope of
        Nothing -> ""
        Just x  -> Data.Text.concat ["custom_collection[published_scope]=", pack $ show x]
    , case sort_order of
        Nothing -> ""
        Just x  -> Data.Text.concat ["custom_collection[sort_order]=", pack $ show x]
    , case template_suffix of
        Nothing -> ""
        Just x  -> Data.Text.concat ["custom_collection[template_suffix]=", pack $ show x]
    , case title of
        Nothing -> ""
        Just x  -> Data.Text.concat ["custom_collection[title]=", pack $ show x]
    , case updated_at of
        Nothing -> ""
        Just x  -> Data.Text.concat ["custom_collection[updated_at]=", pack $ show x]
    ]

instance HasBodyHTML CustomCollection where
  theBodyHTMLMaybe = body_html       
  setBodyHTML x y = y { body_html = Just x }

instance HasHandle CustomCollection where
  theHandleMaybe = handle          
  setHandle x y = y { handle = Just x }

instance HasID CustomCollection where
  theIDMaybe = id              
  setID x y = y { id = Just x }

instance HasImage CustomCollection where
  theImageMaybe = image           
  setImage x y = y { image = Just x }

instance HasPublished CustomCollection where
  thePublishedMaybe = published       
  setPublished x y = y { published = Just x }

instance HasPublishedAt CustomCollection where
  thePublishedAtMaybe = published_at    
  setPublishedAt x y = y { published_at = Just x }

instance HasPublishedScope CustomCollection where
  thePublishedScopeMaybe = published_scope 
  setPublishedScope x y = y { published_scope = Just x }

instance HasSortOrder CustomCollection where
  theSortOrderMaybe = sort_order      
  setSortOrder x y = y { sort_order = Just x }

instance HasTemplateSuffix CustomCollection where
  theTemplateSuffixMaybe = template_suffix 
  setTemplateSuffix x y = y { template_suffix = Just x }

instance HasTitle CustomCollection where
  theTitleMaybe = title           
  setTitle x y = y { title = Just x }

instance HasUpdatedAt CustomCollection where
  theUpdatedAtMaybe = updated_at      
  setUpdatedAt x y = y { updated_at = Just x }

