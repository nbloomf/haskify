{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Haskify.Data.Comment (
  Comment()
  , HasComment, theCommentMaybe, setComment, theComment, HasComments, theCommentsMaybe, setComments, theComments
) where

import Prelude hiding (id, zip)

import Data.Text (Text, pack, concat, intercalate)
import Data.Aeson
import Data.Time.Clock (UTCTime)

import Haskify.Types
import Haskify.DataFields
import Haskify.JSON
import Haskify.HaskifyM

data Comment = Comment
 { article_id   :: Maybe IDNumber
 , author       :: Maybe Text
 , blog_id      :: Maybe IDNumber
 , body         :: Maybe Text
 , body_html    :: Maybe Text
 , created_at   :: Maybe DateTime
 , email        :: Maybe Text
 , id           :: Maybe IDNumber
 , ip           :: Maybe Text
 , published_at :: Maybe DateTime
 , status       :: Maybe Text
 , updated_at   :: Maybe DateTime
 , user_agent   :: Maybe Text
 } deriving (Eq, Show)

class HasComment t where
  theCommentMaybe :: t -> Maybe Comment
  setComment :: Comment -> t -> t

  theComment :: t -> HaskifyM Comment
  theComment x = case theCommentMaybe x of
    Nothing -> fieldDNE "Comment"
    Just y -> return y

class HasComments t where
  theCommentsMaybe :: t -> Maybe [Comment]
  setComments :: [Comment] -> t -> t

  theComments :: t -> HaskifyM [Comment]
  theComments x = case theCommentsMaybe x of
    Nothing -> fieldDNE "Comment"
    Just y -> return y

instance NullObject Comment where
  nullObject = Comment
    { article_id   = Nothing
    , author       = Nothing
    , blog_id      = Nothing
    , body         = Nothing
    , body_html    = Nothing
    , created_at   = Nothing
    , email        = Nothing
    , id           = Nothing
    , ip           = Nothing
    , published_at = Nothing
    , status       = Nothing
    , updated_at   = Nothing
    , user_agent   = Nothing
   }

instance FromJSON Comment where
  parseJSON = withObject "Comment" $ \o -> do
    article_id   <- o .:? "article_id"
    author       <- o .:? "author"
    blog_id      <- o .:? "blog_id"
    body         <- o .:? "body"
    body_html    <- o .:? "body_html"
    created_at   <- o .:? "created_at"
    email        <- o .:? "email"
    id           <- o .:? "id"
    ip           <- o .:? "ip"
    published_at <- o .:? "published_at"
    status       <- o .:? "status"
    updated_at   <- o .:? "updated_at"
    user_agent   <- o .:? "user_agent"
    return Comment{..}


instance ToJSON Comment where
  toJSON Comment{..} = objectSansNull
    [ "article_id"   .= article_id
    , "author"       .= author
    , "blog_id"      .= blog_id
    , "body"         .= body
    , "body_html"    .= body_html
    , "created_at"   .= created_at
    , "email"        .= email
    , "id"           .= id
    , "ip"           .= ip
    , "published_at" .= published_at
    , "status"       .= status
    , "updated_at"   .= updated_at
    , "user_agent"   .= user_agent
    ]


instance ToURLArgs Comment where
  toURLArgs Comment{..} = intercalate "&" $ filter (/= "")
    [ case article_id of
        Nothing -> ""
        Just x  -> Data.Text.concat ["comment[article_id]=", pack $ show x]
    , case author of
        Nothing -> ""
        Just x  -> Data.Text.concat ["comment[author]=", pack $ show x]
    , case blog_id of
        Nothing -> ""
        Just x  -> Data.Text.concat ["comment[blog_id]=", pack $ show x]
    , case body of
        Nothing -> ""
        Just x  -> Data.Text.concat ["comment[body]=", pack $ show x]
    , case body_html of
        Nothing -> ""
        Just x  -> Data.Text.concat ["comment[body_html]=", pack $ show x]
    , case created_at of
        Nothing -> ""
        Just x  -> Data.Text.concat ["comment[created_at]=", pack $ show x]
    , case email of
        Nothing -> ""
        Just x  -> Data.Text.concat ["comment[email]=", pack $ show x]
    , case id of
        Nothing -> ""
        Just x  -> Data.Text.concat ["comment[id]=", pack $ show x]
    , case ip of
        Nothing -> ""
        Just x  -> Data.Text.concat ["comment[ip]=", pack $ show x]
    , case published_at of
        Nothing -> ""
        Just x  -> Data.Text.concat ["comment[published_at]=", pack $ show x]
    , case status of
        Nothing -> ""
        Just x  -> Data.Text.concat ["comment[status]=", pack $ show x]
    , case updated_at of
        Nothing -> ""
        Just x  -> Data.Text.concat ["comment[updated_at]=", pack $ show x]
    , case user_agent of
        Nothing -> ""
        Just x  -> Data.Text.concat ["comment[user_agent]=", pack $ show x]
    ]

instance HasArticleID Comment where
  theArticleIDMaybe = article_id   
  setArticleID x y = y { article_id = Just x }

instance HasAuthor Comment where
  theAuthorMaybe = author       
  setAuthor x y = y { author = Just x }

instance HasBlogID Comment where
  theBlogIDMaybe = blog_id      
  setBlogID x y = y { blog_id = Just x }

instance HasBody Comment where
  theBodyMaybe = body         
  setBody x y = y { body = Just x }

instance HasBodyHTML Comment where
  theBodyHTMLMaybe = body_html    
  setBodyHTML x y = y { body_html = Just x }

instance HasCreatedAt Comment where
  theCreatedAtMaybe = created_at   
  setCreatedAt x y = y { created_at = Just x }

instance HasEmail Comment where
  theEmailMaybe = email        
  setEmail x y = y { email = Just x }

instance HasID Comment where
  theIDMaybe = id           
  setID x y = y { id = Just x }

instance HasIP Comment where
  theIPMaybe = ip           
  setIP x y = y { ip = Just x }

instance HasPublishedAt Comment where
  thePublishedAtMaybe = published_at 
  setPublishedAt x y = y { published_at = Just x }

instance HasStatus Comment where
  theStatusMaybe = status       
  setStatus x y = y { status = Just x }

instance HasUpdatedAt Comment where
  theUpdatedAtMaybe = updated_at   
  setUpdatedAt x y = y { updated_at = Just x }

instance HasUserAgent Comment where
  theUserAgentMaybe = user_agent   
  setUserAgent x y = y { user_agent = Just x }

