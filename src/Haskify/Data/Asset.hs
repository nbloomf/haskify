{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Haskify.Data.Asset (
  Asset()
  , HasAsset, theAssetMaybe, setAsset, theAsset, HasAssets, theAssetsMaybe, setAssets, theAssets
) where

import Prelude hiding (id, zip)

import Data.Text (Text, pack, concat, intercalate)
import Data.Aeson
import Data.Time.Clock (UTCTime)

import Haskify.Types
import Haskify.DataFields
import Haskify.JSON
import Haskify.HaskifyM

data Asset = Asset
 { attachment   :: Maybe Text
 , content_type :: Maybe Text
 , created_at   :: Maybe DateTime
 , key          :: Maybe Text
 , public_url   :: Maybe Text
 , size         :: Maybe Int
 , source_key   :: Maybe Text
 , src          :: Maybe Text
 , theme_id     :: Maybe IDNumber
 , updated_at   :: Maybe DateTime
 , value        :: Maybe Text
 } deriving (Eq, Show)

class HasAsset t where
  theAssetMaybe :: t -> Maybe Asset
  setAsset :: Asset -> t -> t

  theAsset :: t -> HaskifyM Asset
  theAsset x = case theAssetMaybe x of
    Nothing -> fieldDNE "Asset"
    Just y -> return y

class HasAssets t where
  theAssetsMaybe :: t -> Maybe [Asset]
  setAssets :: [Asset] -> t -> t

  theAssets :: t -> HaskifyM [Asset]
  theAssets x = case theAssetsMaybe x of
    Nothing -> fieldDNE "Asset"
    Just y -> return y

instance NullObject Asset where
  nullObject = Asset
    { attachment   = Nothing
    , content_type = Nothing
    , created_at   = Nothing
    , key          = Nothing
    , public_url   = Nothing
    , size         = Nothing
    , source_key   = Nothing
    , src          = Nothing
    , theme_id     = Nothing
    , updated_at   = Nothing
    , value        = Nothing
   }

instance FromJSON Asset where
  parseJSON = withObject "Asset" $ \o -> do
    attachment   <- o .:? "attachment"
    content_type <- o .:? "content_type"
    created_at   <- o .:? "created_at"
    key          <- o .:? "key"
    public_url   <- o .:? "public_url"
    size         <- o .:? "size"
    source_key   <- o .:? "source_key"
    src          <- o .:? "src"
    theme_id     <- o .:? "theme_id"
    updated_at   <- o .:? "updated_at"
    value        <- o .:? "value"
    return Asset{..}


instance ToJSON Asset where
  toJSON Asset{..} = objectSansNull
    [ "attachment"   .= attachment
    , "content_type" .= content_type
    , "created_at"   .= created_at
    , "key"          .= key
    , "public_url"   .= public_url
    , "size"         .= size
    , "source_key"   .= source_key
    , "src"          .= src
    , "theme_id"     .= theme_id
    , "updated_at"   .= updated_at
    , "value"        .= value
    ]


instance ToURLArgs Asset where
  toURLArgs Asset{..} = intercalate "&" $ filter (/= "")
    [ case attachment of
        Nothing -> ""
        Just x  -> Data.Text.concat ["asset[attachment]=", pack $ show x]
    , case content_type of
        Nothing -> ""
        Just x  -> Data.Text.concat ["asset[content_type]=", pack $ show x]
    , case created_at of
        Nothing -> ""
        Just x  -> Data.Text.concat ["asset[created_at]=", pack $ show x]
    , case key of
        Nothing -> ""
        Just x  -> Data.Text.concat ["asset[key]=", pack $ show x]
    , case public_url of
        Nothing -> ""
        Just x  -> Data.Text.concat ["asset[public_url]=", pack $ show x]
    , case size of
        Nothing -> ""
        Just x  -> Data.Text.concat ["asset[size]=", pack $ show x]
    , case source_key of
        Nothing -> ""
        Just x  -> Data.Text.concat ["asset[source_key]=", pack $ show x]
    , case src of
        Nothing -> ""
        Just x  -> Data.Text.concat ["asset[src]=", pack $ show x]
    , case theme_id of
        Nothing -> ""
        Just x  -> Data.Text.concat ["asset[theme_id]=", pack $ show x]
    , case updated_at of
        Nothing -> ""
        Just x  -> Data.Text.concat ["asset[updated_at]=", pack $ show x]
    , case value of
        Nothing -> ""
        Just x  -> Data.Text.concat ["asset[value]=", pack $ show x]
    ]

instance HasAttachment Asset where
  theAttachmentMaybe = attachment   
  setAttachment x y = y { attachment = Just x }

instance HasContentType Asset where
  theContentTypeMaybe = content_type 
  setContentType x y = y { content_type = Just x }

instance HasCreatedAt Asset where
  theCreatedAtMaybe = created_at   
  setCreatedAt x y = y { created_at = Just x }

instance HasKey Asset where
  theKeyMaybe = key          
  setKey x y = y { key = Just x }

instance HasPublicURL Asset where
  thePublicURLMaybe = public_url   
  setPublicURL x y = y { public_url = Just x }

instance HasSize Asset where
  theSizeMaybe = size         
  setSize x y = y { size = Just x }

instance HasSourceKey Asset where
  theSourceKeyMaybe = source_key   
  setSourceKey x y = y { source_key = Just x }

instance HasSrc Asset where
  theSrcMaybe = src          
  setSrc x y = y { src = Just x }

instance HasThemeID Asset where
  theThemeIDMaybe = theme_id     
  setThemeID x y = y { theme_id = Just x }

instance HasUpdatedAt Asset where
  theUpdatedAtMaybe = updated_at   
  setUpdatedAt x y = y { updated_at = Just x }

instance HasValue Asset where
  theValueMaybe = value        
  setValue x y = y { value = Just x }

