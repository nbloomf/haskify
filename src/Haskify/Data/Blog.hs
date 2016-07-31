{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Haskify.Data.Blog (
  Blog()
  , HasBlog, theBlogMaybe, setBlog, theBlog, HasBlogs, theBlogsMaybe, setBlogs, theBlogs
) where

import Prelude hiding (id, zip)

import Data.Text (Text, pack, concat, intercalate)
import Data.Aeson
import Data.Time.Clock (UTCTime)

import Haskify.Types
import Haskify.DataFields
import Haskify.JSON
import Haskify.HaskifyM
import Haskify.Value.Commentable

data Blog = Blog
 { commentable         :: Maybe Commentable
 , created_at          :: Maybe DateTime
 , feedburner          :: Maybe Text
 , feedburner_location :: Maybe Text
 , handle              :: Maybe Text
 , id                  :: Maybe IDNumber
 , tags                :: Maybe Text
 , template_suffix     :: Maybe Text
 , title               :: Maybe Text
 , updated_at          :: Maybe DateTime
 } deriving (Eq, Show)

class HasBlog t where
  theBlogMaybe :: t -> Maybe Blog
  setBlog :: Blog -> t -> t

  theBlog :: t -> HaskifyM Blog
  theBlog x = case theBlogMaybe x of
    Nothing -> fieldDNE "Blog"
    Just y -> return y

class HasBlogs t where
  theBlogsMaybe :: t -> Maybe [Blog]
  setBlogs :: [Blog] -> t -> t

  theBlogs :: t -> HaskifyM [Blog]
  theBlogs x = case theBlogsMaybe x of
    Nothing -> fieldDNE "Blog"
    Just y -> return y

instance NullObject Blog where
  nullObject = Blog
    { commentable         = Nothing
    , created_at          = Nothing
    , feedburner          = Nothing
    , feedburner_location = Nothing
    , handle              = Nothing
    , id                  = Nothing
    , tags                = Nothing
    , template_suffix     = Nothing
    , title               = Nothing
    , updated_at          = Nothing
   }

instance FromJSON Blog where
  parseJSON = withObject "Blog" $ \o -> do
    commentable         <- o .:? "commentable"
    created_at          <- o .:? "created_at"
    feedburner          <- o .:? "feedburner"
    feedburner_location <- o .:? "feedburner_location"
    handle              <- o .:? "handle"
    id                  <- o .:? "id"
    tags                <- o .:? "tags"
    template_suffix     <- o .:? "template_suffix"
    title               <- o .:? "title"
    updated_at          <- o .:? "updated_at"
    return Blog{..}


instance ToJSON Blog where
  toJSON Blog{..} = objectSansNull
    [ "commentable"         .= commentable
    , "created_at"          .= created_at
    , "feedburner"          .= feedburner
    , "feedburner_location" .= feedburner_location
    , "handle"              .= handle
    , "id"                  .= id
    , "tags"                .= tags
    , "template_suffix"     .= template_suffix
    , "title"               .= title
    , "updated_at"          .= updated_at
    ]


instance ToURLArgs Blog where
  toURLArgs Blog{..} = intercalate "&" $ filter (/= "")
    [ case commentable of
        Nothing -> ""
        Just x  -> Data.Text.concat ["blog[commentable]=", pack $ show x]
    , case created_at of
        Nothing -> ""
        Just x  -> Data.Text.concat ["blog[created_at]=", pack $ show x]
    , case feedburner of
        Nothing -> ""
        Just x  -> Data.Text.concat ["blog[feedburner]=", pack $ show x]
    , case feedburner_location of
        Nothing -> ""
        Just x  -> Data.Text.concat ["blog[feedburner_location]=", pack $ show x]
    , case handle of
        Nothing -> ""
        Just x  -> Data.Text.concat ["blog[handle]=", pack $ show x]
    , case id of
        Nothing -> ""
        Just x  -> Data.Text.concat ["blog[id]=", pack $ show x]
    , case tags of
        Nothing -> ""
        Just x  -> Data.Text.concat ["blog[tags]=", pack $ show x]
    , case template_suffix of
        Nothing -> ""
        Just x  -> Data.Text.concat ["blog[template_suffix]=", pack $ show x]
    , case title of
        Nothing -> ""
        Just x  -> Data.Text.concat ["blog[title]=", pack $ show x]
    , case updated_at of
        Nothing -> ""
        Just x  -> Data.Text.concat ["blog[updated_at]=", pack $ show x]
    ]

instance HasCommentable Blog where
  theCommentableMaybe = commentable         
  setCommentable x y = y { commentable = Just x }

instance HasCreatedAt Blog where
  theCreatedAtMaybe = created_at          
  setCreatedAt x y = y { created_at = Just x }

instance HasFeedburner Blog where
  theFeedburnerMaybe = feedburner          
  setFeedburner x y = y { feedburner = Just x }

instance HasFeedburnerLocation Blog where
  theFeedburnerLocationMaybe = feedburner_location 
  setFeedburnerLocation x y = y { feedburner_location = Just x }

instance HasHandle Blog where
  theHandleMaybe = handle              
  setHandle x y = y { handle = Just x }

instance HasID Blog where
  theIDMaybe = id                  
  setID x y = y { id = Just x }

instance HasTags Blog where
  theTagsMaybe = tags                
  setTags x y = y { tags = Just x }

instance HasTemplateSuffix Blog where
  theTemplateSuffixMaybe = template_suffix     
  setTemplateSuffix x y = y { template_suffix = Just x }

instance HasTitle Blog where
  theTitleMaybe = title               
  setTitle x y = y { title = Just x }

instance HasUpdatedAt Blog where
  theUpdatedAtMaybe = updated_at          
  setUpdatedAt x y = y { updated_at = Just x }

