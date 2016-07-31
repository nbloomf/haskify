{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Haskify.Data.Page (
  Page()
  , HasPage, thePageMaybe, setPage, thePage, HasPages, thePagesMaybe, setPages, thePages
) where

import Prelude hiding (id, zip)

import Data.Text (Text, pack, concat, intercalate)
import Data.Aeson
import Data.Time.Clock (UTCTime)

import Haskify.Types
import Haskify.DataFields
import Haskify.JSON
import Haskify.HaskifyM

data Page = Page
 { author          :: Maybe Text
 , body_html       :: Maybe Text
 , created_at      :: Maybe DateTime
 , handle          :: Maybe Text
 , id              :: Maybe IDNumber
 , published_at    :: Maybe DateTime
 , shop_id         :: Maybe IDNumber
 , template_suffix :: Maybe Text
 , title           :: Maybe Text
 , updated_at      :: Maybe DateTime
 } deriving (Eq, Show)

class HasPage t where
  thePageMaybe :: t -> Maybe Page
  setPage :: Page -> t -> t

  thePage :: t -> HaskifyM Page
  thePage x = case thePageMaybe x of
    Nothing -> fieldDNE "Page"
    Just y -> return y

class HasPages t where
  thePagesMaybe :: t -> Maybe [Page]
  setPages :: [Page] -> t -> t

  thePages :: t -> HaskifyM [Page]
  thePages x = case thePagesMaybe x of
    Nothing -> fieldDNE "Page"
    Just y -> return y

instance NullObject Page where
  nullObject = Page
    { author          = Nothing
    , body_html       = Nothing
    , created_at      = Nothing
    , handle          = Nothing
    , id              = Nothing
    , published_at    = Nothing
    , shop_id         = Nothing
    , template_suffix = Nothing
    , title           = Nothing
    , updated_at      = Nothing
   }

instance FromJSON Page where
  parseJSON = withObject "Page" $ \o -> do
    author          <- o .:? "author"
    body_html       <- o .:? "body_html"
    created_at      <- o .:? "created_at"
    handle          <- o .:? "handle"
    id              <- o .:? "id"
    published_at    <- o .:? "published_at"
    shop_id         <- o .:? "shop_id"
    template_suffix <- o .:? "template_suffix"
    title           <- o .:? "title"
    updated_at      <- o .:? "updated_at"
    return Page{..}


instance ToJSON Page where
  toJSON Page{..} = objectSansNull
    [ "author"          .= author
    , "body_html"       .= body_html
    , "created_at"      .= created_at
    , "handle"          .= handle
    , "id"              .= id
    , "published_at"    .= published_at
    , "shop_id"         .= shop_id
    , "template_suffix" .= template_suffix
    , "title"           .= title
    , "updated_at"      .= updated_at
    ]


instance ToURLArgs Page where
  toURLArgs Page{..} = intercalate "&" $ filter (/= "")
    [ case author of
        Nothing -> ""
        Just x  -> Data.Text.concat ["page[author]=", pack $ show x]
    , case body_html of
        Nothing -> ""
        Just x  -> Data.Text.concat ["page[body_html]=", pack $ show x]
    , case created_at of
        Nothing -> ""
        Just x  -> Data.Text.concat ["page[created_at]=", pack $ show x]
    , case handle of
        Nothing -> ""
        Just x  -> Data.Text.concat ["page[handle]=", pack $ show x]
    , case id of
        Nothing -> ""
        Just x  -> Data.Text.concat ["page[id]=", pack $ show x]
    , case published_at of
        Nothing -> ""
        Just x  -> Data.Text.concat ["page[published_at]=", pack $ show x]
    , case shop_id of
        Nothing -> ""
        Just x  -> Data.Text.concat ["page[shop_id]=", pack $ show x]
    , case template_suffix of
        Nothing -> ""
        Just x  -> Data.Text.concat ["page[template_suffix]=", pack $ show x]
    , case title of
        Nothing -> ""
        Just x  -> Data.Text.concat ["page[title]=", pack $ show x]
    , case updated_at of
        Nothing -> ""
        Just x  -> Data.Text.concat ["page[updated_at]=", pack $ show x]
    ]

instance HasAuthor Page where
  theAuthorMaybe = author          
  setAuthor x y = y { author = Just x }

instance HasBodyHTML Page where
  theBodyHTMLMaybe = body_html       
  setBodyHTML x y = y { body_html = Just x }

instance HasCreatedAt Page where
  theCreatedAtMaybe = created_at      
  setCreatedAt x y = y { created_at = Just x }

instance HasHandle Page where
  theHandleMaybe = handle          
  setHandle x y = y { handle = Just x }

instance HasID Page where
  theIDMaybe = id              
  setID x y = y { id = Just x }

instance HasPublishedAt Page where
  thePublishedAtMaybe = published_at    
  setPublishedAt x y = y { published_at = Just x }

instance HasShopID Page where
  theShopIDMaybe = shop_id         
  setShopID x y = y { shop_id = Just x }

instance HasTemplateSuffix Page where
  theTemplateSuffixMaybe = template_suffix 
  setTemplateSuffix x y = y { template_suffix = Just x }

instance HasTitle Page where
  theTitleMaybe = title           
  setTitle x y = y { title = Just x }

instance HasUpdatedAt Page where
  theUpdatedAtMaybe = updated_at      
  setUpdatedAt x y = y { updated_at = Just x }

