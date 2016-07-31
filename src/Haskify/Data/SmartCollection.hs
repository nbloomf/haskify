{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Haskify.Data.SmartCollection (
  SmartCollection()
  , HasSmartCollection, theSmartCollectionMaybe, setSmartCollection, theSmartCollection, HasSmartCollections, theSmartCollectionsMaybe, setSmartCollections, theSmartCollections
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
import Haskify.Data.Rule

data SmartCollection = SmartCollection
 { body_html       :: Maybe Text
 , handle          :: Maybe Text
 , id              :: Maybe IDNumber
 , image           :: Maybe Image
 , published_at    :: Maybe DateTime
 , published_scope :: Maybe PublishedScope
 , rules           :: Maybe [Rule]
 , disjunctive     :: Maybe Bool
 , sort_order      :: Maybe SortOrder
 , template_suffix :: Maybe Text
 , title           :: Maybe Text
 , updated_at      :: Maybe DateTime
 } deriving (Eq, Show)

class HasSmartCollection t where
  theSmartCollectionMaybe :: t -> Maybe SmartCollection
  setSmartCollection :: SmartCollection -> t -> t

  theSmartCollection :: t -> HaskifyM SmartCollection
  theSmartCollection x = case theSmartCollectionMaybe x of
    Nothing -> fieldDNE "SmartCollection"
    Just y -> return y

class HasSmartCollections t where
  theSmartCollectionsMaybe :: t -> Maybe [SmartCollection]
  setSmartCollections :: [SmartCollection] -> t -> t

  theSmartCollections :: t -> HaskifyM [SmartCollection]
  theSmartCollections x = case theSmartCollectionsMaybe x of
    Nothing -> fieldDNE "SmartCollection"
    Just y -> return y

instance NullObject SmartCollection where
  nullObject = SmartCollection
    { body_html       = Nothing
    , handle          = Nothing
    , id              = Nothing
    , image           = Nothing
    , published_at    = Nothing
    , published_scope = Nothing
    , rules           = Nothing
    , disjunctive     = Nothing
    , sort_order      = Nothing
    , template_suffix = Nothing
    , title           = Nothing
    , updated_at      = Nothing
   }

instance FromJSON SmartCollection where
  parseJSON = withObject "SmartCollection" $ \o -> do
    body_html       <- o .:? "body_html"
    handle          <- o .:? "handle"
    id              <- o .:? "id"
    image           <- o .:? "image"
    published_at    <- o .:? "published_at"
    published_scope <- o .:? "published_scope"
    rules           <- o .:? "rules"
    disjunctive     <- o .:? "disjunctive"
    sort_order      <- o .:? "sort_order"
    template_suffix <- o .:? "template_suffix"
    title           <- o .:? "title"
    updated_at      <- o .:? "updated_at"
    return SmartCollection{..}


instance ToJSON SmartCollection where
  toJSON SmartCollection{..} = objectSansNull
    [ "body_html"       .= body_html
    , "handle"          .= handle
    , "id"              .= id
    , "image"           .= image
    , "published_at"    .= published_at
    , "published_scope" .= published_scope
    , "rules"           .= rules
    , "disjunctive"     .= disjunctive
    , "sort_order"      .= sort_order
    , "template_suffix" .= template_suffix
    , "title"           .= title
    , "updated_at"      .= updated_at
    ]


instance ToURLArgs SmartCollection where
  toURLArgs SmartCollection{..} = intercalate "&" $ filter (/= "")
    [ case body_html of
        Nothing -> ""
        Just x  -> Data.Text.concat ["smart_collection[body_html]=", pack $ show x]
    , case handle of
        Nothing -> ""
        Just x  -> Data.Text.concat ["smart_collection[handle]=", pack $ show x]
    , case id of
        Nothing -> ""
        Just x  -> Data.Text.concat ["smart_collection[id]=", pack $ show x]
    , case image of
        Nothing -> ""
        Just x  -> Data.Text.concat ["smart_collection[image]=", pack $ show x]
    , case published_at of
        Nothing -> ""
        Just x  -> Data.Text.concat ["smart_collection[published_at]=", pack $ show x]
    , case published_scope of
        Nothing -> ""
        Just x  -> Data.Text.concat ["smart_collection[published_scope]=", pack $ show x]
    , case rules of
        Nothing -> ""
        Just x  -> Data.Text.concat ["smart_collection[rules]=", pack $ show x]
    , case disjunctive of
        Nothing -> ""
        Just x  -> Data.Text.concat ["smart_collection[disjunctive]=", pack $ show x]
    , case sort_order of
        Nothing -> ""
        Just x  -> Data.Text.concat ["smart_collection[sort_order]=", pack $ show x]
    , case template_suffix of
        Nothing -> ""
        Just x  -> Data.Text.concat ["smart_collection[template_suffix]=", pack $ show x]
    , case title of
        Nothing -> ""
        Just x  -> Data.Text.concat ["smart_collection[title]=", pack $ show x]
    , case updated_at of
        Nothing -> ""
        Just x  -> Data.Text.concat ["smart_collection[updated_at]=", pack $ show x]
    ]

instance HasBodyHTML SmartCollection where
  theBodyHTMLMaybe = body_html       
  setBodyHTML x y = y { body_html = Just x }

instance HasHandle SmartCollection where
  theHandleMaybe = handle          
  setHandle x y = y { handle = Just x }

instance HasID SmartCollection where
  theIDMaybe = id              
  setID x y = y { id = Just x }

instance HasImage SmartCollection where
  theImageMaybe = image           
  setImage x y = y { image = Just x }

instance HasPublishedAt SmartCollection where
  thePublishedAtMaybe = published_at    
  setPublishedAt x y = y { published_at = Just x }

instance HasPublishedScope SmartCollection where
  thePublishedScopeMaybe = published_scope 
  setPublishedScope x y = y { published_scope = Just x }

instance HasRules SmartCollection where
  theRulesMaybe = rules           
  setRules x y = y { rules = Just x }

instance HasDisjunctive SmartCollection where
  theDisjunctiveMaybe = disjunctive     
  setDisjunctive x y = y { disjunctive = Just x }

instance HasSortOrder SmartCollection where
  theSortOrderMaybe = sort_order      
  setSortOrder x y = y { sort_order = Just x }

instance HasTemplateSuffix SmartCollection where
  theTemplateSuffixMaybe = template_suffix 
  setTemplateSuffix x y = y { template_suffix = Just x }

instance HasTitle SmartCollection where
  theTitleMaybe = title           
  setTitle x y = y { title = Just x }

instance HasUpdatedAt SmartCollection where
  theUpdatedAtMaybe = updated_at      
  setUpdatedAt x y = y { updated_at = Just x }

