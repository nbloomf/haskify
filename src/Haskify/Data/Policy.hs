{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Haskify.Data.Policy (
  Policy()
  , HasPolicy, thePolicyMaybe, setPolicy, thePolicy, HasPolicys, thePolicysMaybe, setPolicys, thePolicys
) where

import Prelude hiding (id, zip)

import Data.Text (Text, pack, concat, intercalate)
import Data.Aeson
import Data.Time.Clock (UTCTime)

import Haskify.Types
import Haskify.DataFields
import Haskify.JSON
import Haskify.HaskifyM

data Policy = Policy
 { title      :: Maybe Text
 , body       :: Maybe Text
 , created_at :: Maybe DateTime
 , updated_at :: Maybe DateTime
 , url        :: Maybe Text
 } deriving (Eq, Show)

class HasPolicy t where
  thePolicyMaybe :: t -> Maybe Policy
  setPolicy :: Policy -> t -> t

  thePolicy :: t -> HaskifyM Policy
  thePolicy x = case thePolicyMaybe x of
    Nothing -> fieldDNE "Policy"
    Just y -> return y

class HasPolicys t where
  thePolicysMaybe :: t -> Maybe [Policy]
  setPolicys :: [Policy] -> t -> t

  thePolicys :: t -> HaskifyM [Policy]
  thePolicys x = case thePolicysMaybe x of
    Nothing -> fieldDNE "Policy"
    Just y -> return y

instance NullObject Policy where
  nullObject = Policy
    { title      = Nothing
    , body       = Nothing
    , created_at = Nothing
    , updated_at = Nothing
    , url        = Nothing
   }

instance FromJSON Policy where
  parseJSON = withObject "Policy" $ \o -> do
    title      <- o .:? "title"
    body       <- o .:? "body"
    created_at <- o .:? "created_at"
    updated_at <- o .:? "updated_at"
    url        <- o .:? "url"
    return Policy{..}


instance ToJSON Policy where
  toJSON Policy{..} = objectSansNull
    [ "title"      .= title
    , "body"       .= body
    , "created_at" .= created_at
    , "updated_at" .= updated_at
    , "url"        .= url
    ]


instance ToURLArgs Policy where
  toURLArgs Policy{..} = intercalate "&" $ filter (/= "")
    [ case title of
        Nothing -> ""
        Just x  -> Data.Text.concat ["policy[title]=", pack $ show x]
    , case body of
        Nothing -> ""
        Just x  -> Data.Text.concat ["policy[body]=", pack $ show x]
    , case created_at of
        Nothing -> ""
        Just x  -> Data.Text.concat ["policy[created_at]=", pack $ show x]
    , case updated_at of
        Nothing -> ""
        Just x  -> Data.Text.concat ["policy[updated_at]=", pack $ show x]
    , case url of
        Nothing -> ""
        Just x  -> Data.Text.concat ["policy[url]=", pack $ show x]
    ]

instance HasTitle Policy where
  theTitleMaybe = title      
  setTitle x y = y { title = Just x }

instance HasBody Policy where
  theBodyMaybe = body       
  setBody x y = y { body = Just x }

instance HasCreatedAt Policy where
  theCreatedAtMaybe = created_at 
  setCreatedAt x y = y { created_at = Just x }

instance HasUpdatedAt Policy where
  theUpdatedAtMaybe = updated_at 
  setUpdatedAt x y = y { updated_at = Just x }

instance HasURL Policy where
  theURLMaybe = url        
  setURL x y = y { url = Just x }

