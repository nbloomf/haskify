{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Haskify.Data.Article (
  Article()
  , HasArticle, theArticleMaybe, setArticle, theArticle, HasArticles, theArticlesMaybe, setArticles, theArticles
) where

import Prelude hiding (id, zip)

import Data.Text (Text, pack, concat, intercalate)
import Data.Aeson
import Data.Time.Clock (UTCTime)

import Haskify.Types
import Haskify.DataFields
import Haskify.JSON
import Haskify.HaskifyM
import Haskify.Data.Image

data Article = Article
 { author          :: Maybe Text
 , blog_id         :: Maybe IDNumber
 , body_html       :: Maybe Text
 , created_at      :: Maybe DateTime
 , id              :: Maybe IDNumber
 , image           :: Maybe Image
 , published       :: Maybe Bool
 , published_at    :: Maybe DateTime
 , summary_html    :: Maybe Text
 , tags            :: Maybe Text
 , template_suffix :: Maybe Text
 , title           :: Maybe Text
 , updated_at      :: Maybe DateTime
 , user_id         :: Maybe IDNumber
 } deriving (Eq, Show)

class HasArticle t where
  theArticleMaybe :: t -> Maybe Article
  setArticle :: Article -> t -> t

  theArticle :: t -> HaskifyM Article
  theArticle x = case theArticleMaybe x of
    Nothing -> fieldDNE "Article"
    Just y -> return y

class HasArticles t where
  theArticlesMaybe :: t -> Maybe [Article]
  setArticles :: [Article] -> t -> t

  theArticles :: t -> HaskifyM [Article]
  theArticles x = case theArticlesMaybe x of
    Nothing -> fieldDNE "Article"
    Just y -> return y

instance NullObject Article where
  nullObject = Article
    { author          = Nothing
    , blog_id         = Nothing
    , body_html       = Nothing
    , created_at      = Nothing
    , id              = Nothing
    , image           = Nothing
    , published       = Nothing
    , published_at    = Nothing
    , summary_html    = Nothing
    , tags            = Nothing
    , template_suffix = Nothing
    , title           = Nothing
    , updated_at      = Nothing
    , user_id         = Nothing
   }

instance FromJSON Article where
  parseJSON = withObject "Article" $ \o -> do
    author          <- o .:? "author"
    blog_id         <- o .:? "blog_id"
    body_html       <- o .:? "body_html"
    created_at      <- o .:? "created_at"
    id              <- o .:? "id"
    image           <- o .:? "image"
    published       <- o .:? "published"
    published_at    <- o .:? "published_at"
    summary_html    <- o .:? "summary_html"
    tags            <- o .:? "tags"
    template_suffix <- o .:? "template_suffix"
    title           <- o .:? "title"
    updated_at      <- o .:? "updated_at"
    user_id         <- o .:? "user_id"
    return Article{..}


instance ToJSON Article where
  toJSON Article{..} = objectSansNull
    [ "author"          .= author
    , "blog_id"         .= blog_id
    , "body_html"       .= body_html
    , "created_at"      .= created_at
    , "id"              .= id
    , "image"           .= image
    , "published"       .= published
    , "published_at"    .= published_at
    , "summary_html"    .= summary_html
    , "tags"            .= tags
    , "template_suffix" .= template_suffix
    , "title"           .= title
    , "updated_at"      .= updated_at
    , "user_id"         .= user_id
    ]


instance ToURLArgs Article where
  toURLArgs Article{..} = intercalate "&" $ filter (/= "")
    [ case author of
        Nothing -> ""
        Just x  -> Data.Text.concat ["article[author]=", pack $ show x]
    , case blog_id of
        Nothing -> ""
        Just x  -> Data.Text.concat ["article[blog_id]=", pack $ show x]
    , case body_html of
        Nothing -> ""
        Just x  -> Data.Text.concat ["article[body_html]=", pack $ show x]
    , case created_at of
        Nothing -> ""
        Just x  -> Data.Text.concat ["article[created_at]=", pack $ show x]
    , case id of
        Nothing -> ""
        Just x  -> Data.Text.concat ["article[id]=", pack $ show x]
    , case image of
        Nothing -> ""
        Just x  -> Data.Text.concat ["article[image]=", pack $ show x]
    , case published of
        Nothing -> ""
        Just x  -> Data.Text.concat ["article[published]=", pack $ show x]
    , case published_at of
        Nothing -> ""
        Just x  -> Data.Text.concat ["article[published_at]=", pack $ show x]
    , case summary_html of
        Nothing -> ""
        Just x  -> Data.Text.concat ["article[summary_html]=", pack $ show x]
    , case tags of
        Nothing -> ""
        Just x  -> Data.Text.concat ["article[tags]=", pack $ show x]
    , case template_suffix of
        Nothing -> ""
        Just x  -> Data.Text.concat ["article[template_suffix]=", pack $ show x]
    , case title of
        Nothing -> ""
        Just x  -> Data.Text.concat ["article[title]=", pack $ show x]
    , case updated_at of
        Nothing -> ""
        Just x  -> Data.Text.concat ["article[updated_at]=", pack $ show x]
    , case user_id of
        Nothing -> ""
        Just x  -> Data.Text.concat ["article[user_id]=", pack $ show x]
    ]

instance HasAuthor Article where
  theAuthorMaybe = author          
  setAuthor x y = y { author = Just x }

instance HasBlogID Article where
  theBlogIDMaybe = blog_id         
  setBlogID x y = y { blog_id = Just x }

instance HasBodyHTML Article where
  theBodyHTMLMaybe = body_html       
  setBodyHTML x y = y { body_html = Just x }

instance HasCreatedAt Article where
  theCreatedAtMaybe = created_at      
  setCreatedAt x y = y { created_at = Just x }

instance HasID Article where
  theIDMaybe = id              
  setID x y = y { id = Just x }

instance HasImage Article where
  theImageMaybe = image           
  setImage x y = y { image = Just x }

instance HasPublished Article where
  thePublishedMaybe = published       
  setPublished x y = y { published = Just x }

instance HasPublishedAt Article where
  thePublishedAtMaybe = published_at    
  setPublishedAt x y = y { published_at = Just x }

instance HasSummaryHTML Article where
  theSummaryHTMLMaybe = summary_html    
  setSummaryHTML x y = y { summary_html = Just x }

instance HasTags Article where
  theTagsMaybe = tags            
  setTags x y = y { tags = Just x }

instance HasTemplateSuffix Article where
  theTemplateSuffixMaybe = template_suffix 
  setTemplateSuffix x y = y { template_suffix = Just x }

instance HasTitle Article where
  theTitleMaybe = title           
  setTitle x y = y { title = Just x }

instance HasUpdatedAt Article where
  theUpdatedAtMaybe = updated_at      
  setUpdatedAt x y = y { updated_at = Just x }

instance HasUserID Article where
  theUserIDMaybe = user_id         
  setUserID x y = y { user_id = Just x }

