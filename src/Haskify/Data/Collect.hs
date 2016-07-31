{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Haskify.Data.Collect (
  Collect()
  , HasCollect, theCollectMaybe, setCollect, theCollect, HasCollects, theCollectsMaybe, setCollects, theCollects
) where

import Prelude hiding (id, zip)

import Data.Text (Text, pack, concat, intercalate)
import Data.Aeson
import Data.Time.Clock (UTCTime)

import Haskify.Types
import Haskify.DataFields
import Haskify.JSON
import Haskify.HaskifyM

data Collect = Collect
 { collection_id :: Maybe IDNumber
 , created_at    :: Maybe DateTime
 , featured      :: Maybe Bool
 , id            :: Maybe IDNumber
 , position      :: Maybe Int
 , product_id    :: Maybe IDNumber
 , sort_value    :: Maybe Text
 , updated_at    :: Maybe DateTime
 } deriving (Eq, Show)

class HasCollect t where
  theCollectMaybe :: t -> Maybe Collect
  setCollect :: Collect -> t -> t

  theCollect :: t -> HaskifyM Collect
  theCollect x = case theCollectMaybe x of
    Nothing -> fieldDNE "Collect"
    Just y -> return y

class HasCollects t where
  theCollectsMaybe :: t -> Maybe [Collect]
  setCollects :: [Collect] -> t -> t

  theCollects :: t -> HaskifyM [Collect]
  theCollects x = case theCollectsMaybe x of
    Nothing -> fieldDNE "Collect"
    Just y -> return y

instance NullObject Collect where
  nullObject = Collect
    { collection_id = Nothing
    , created_at    = Nothing
    , featured      = Nothing
    , id            = Nothing
    , position      = Nothing
    , product_id    = Nothing
    , sort_value    = Nothing
    , updated_at    = Nothing
   }

instance FromJSON Collect where
  parseJSON = withObject "Collect" $ \o -> do
    collection_id <- o .:? "collection_id"
    created_at    <- o .:? "created_at"
    featured      <- o .:? "featured"
    id            <- o .:? "id"
    position      <- o .:? "position"
    product_id    <- o .:? "product_id"
    sort_value    <- o .:? "sort_value"
    updated_at    <- o .:? "updated_at"
    return Collect{..}


instance ToJSON Collect where
  toJSON Collect{..} = objectSansNull
    [ "collection_id" .= collection_id
    , "created_at"    .= created_at
    , "featured"      .= featured
    , "id"            .= id
    , "position"      .= position
    , "product_id"    .= product_id
    , "sort_value"    .= sort_value
    , "updated_at"    .= updated_at
    ]


instance ToURLArgs Collect where
  toURLArgs Collect{..} = intercalate "&" $ filter (/= "")
    [ case collection_id of
        Nothing -> ""
        Just x  -> Data.Text.concat ["collect[collection_id]=", pack $ show x]
    , case created_at of
        Nothing -> ""
        Just x  -> Data.Text.concat ["collect[created_at]=", pack $ show x]
    , case featured of
        Nothing -> ""
        Just x  -> Data.Text.concat ["collect[featured]=", pack $ show x]
    , case id of
        Nothing -> ""
        Just x  -> Data.Text.concat ["collect[id]=", pack $ show x]
    , case position of
        Nothing -> ""
        Just x  -> Data.Text.concat ["collect[position]=", pack $ show x]
    , case product_id of
        Nothing -> ""
        Just x  -> Data.Text.concat ["collect[product_id]=", pack $ show x]
    , case sort_value of
        Nothing -> ""
        Just x  -> Data.Text.concat ["collect[sort_value]=", pack $ show x]
    , case updated_at of
        Nothing -> ""
        Just x  -> Data.Text.concat ["collect[updated_at]=", pack $ show x]
    ]

instance HasCollectionID Collect where
  theCollectionIDMaybe = collection_id 
  setCollectionID x y = y { collection_id = Just x }

instance HasCreatedAt Collect where
  theCreatedAtMaybe = created_at    
  setCreatedAt x y = y { created_at = Just x }

instance HasFeatured Collect where
  theFeaturedMaybe = featured      
  setFeatured x y = y { featured = Just x }

instance HasID Collect where
  theIDMaybe = id            
  setID x y = y { id = Just x }

instance HasPosition Collect where
  thePositionMaybe = position      
  setPosition x y = y { position = Just x }

instance HasProductID Collect where
  theProductIDMaybe = product_id    
  setProductID x y = y { product_id = Just x }

instance HasSortValue Collect where
  theSortValueMaybe = sort_value    
  setSortValue x y = y { sort_value = Just x }

instance HasUpdatedAt Collect where
  theUpdatedAtMaybe = updated_at    
  setUpdatedAt x y = y { updated_at = Just x }

