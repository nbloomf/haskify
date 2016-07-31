{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Haskify.Data.Metafield (
  Metafield()
  , HasMetafield, theMetafieldMaybe, setMetafield, theMetafield, HasMetafields, theMetafieldsMaybe, setMetafields, theMetafields
) where

import Prelude hiding (id, zip)

import Data.Text (Text, pack, concat, intercalate)
import Data.Aeson
import Data.Time.Clock (UTCTime)

import Haskify.Types
import Haskify.DataFields
import Haskify.JSON
import Haskify.HaskifyM
import Haskify.Value.ValueType

data Metafield = Metafield
 { created_at     :: Maybe DateTime
 , description    :: Maybe Text
 , id             :: Maybe IDNumber
 , key            :: Maybe Text
 , namespace      :: Maybe Text
 , owner_id       :: Maybe IDNumber
 , owner_resource :: Maybe Text
 , value          :: Maybe Text
 , value_type     :: Maybe ValueType
 , updated_at     :: Maybe DateTime
 } deriving (Eq, Show)

class HasMetafield t where
  theMetafieldMaybe :: t -> Maybe Metafield
  setMetafield :: Metafield -> t -> t

  theMetafield :: t -> HaskifyM Metafield
  theMetafield x = case theMetafieldMaybe x of
    Nothing -> fieldDNE "Metafield"
    Just y -> return y

class HasMetafields t where
  theMetafieldsMaybe :: t -> Maybe [Metafield]
  setMetafields :: [Metafield] -> t -> t

  theMetafields :: t -> HaskifyM [Metafield]
  theMetafields x = case theMetafieldsMaybe x of
    Nothing -> fieldDNE "Metafield"
    Just y -> return y

instance NullObject Metafield where
  nullObject = Metafield
    { created_at     = Nothing
    , description    = Nothing
    , id             = Nothing
    , key            = Nothing
    , namespace      = Nothing
    , owner_id       = Nothing
    , owner_resource = Nothing
    , value          = Nothing
    , value_type     = Nothing
    , updated_at     = Nothing
   }

instance FromJSON Metafield where
  parseJSON = withObject "Metafield" $ \o -> do
    created_at     <- o .:? "created_at"
    description    <- o .:? "description"
    id             <- o .:? "id"
    key            <- o .:? "key"
    namespace      <- o .:? "namespace"
    owner_id       <- o .:? "owner_id"
    owner_resource <- o .:? "owner_resource"
    value          <- o .:? "value"
    value_type     <- o .:? "value_type"
    updated_at     <- o .:? "updated_at"
    return Metafield{..}


instance ToJSON Metafield where
  toJSON Metafield{..} = objectSansNull
    [ "created_at"     .= created_at
    , "description"    .= description
    , "id"             .= id
    , "key"            .= key
    , "namespace"      .= namespace
    , "owner_id"       .= owner_id
    , "owner_resource" .= owner_resource
    , "value"          .= value
    , "value_type"     .= value_type
    , "updated_at"     .= updated_at
    ]


instance ToURLArgs Metafield where
  toURLArgs Metafield{..} = intercalate "&" $ filter (/= "")
    [ case created_at of
        Nothing -> ""
        Just x  -> Data.Text.concat ["metafield[created_at]=", pack $ show x]
    , case description of
        Nothing -> ""
        Just x  -> Data.Text.concat ["metafield[description]=", pack $ show x]
    , case id of
        Nothing -> ""
        Just x  -> Data.Text.concat ["metafield[id]=", pack $ show x]
    , case key of
        Nothing -> ""
        Just x  -> Data.Text.concat ["metafield[key]=", pack $ show x]
    , case namespace of
        Nothing -> ""
        Just x  -> Data.Text.concat ["metafield[namespace]=", pack $ show x]
    , case owner_id of
        Nothing -> ""
        Just x  -> Data.Text.concat ["metafield[owner_id]=", pack $ show x]
    , case owner_resource of
        Nothing -> ""
        Just x  -> Data.Text.concat ["metafield[owner_resource]=", pack $ show x]
    , case value of
        Nothing -> ""
        Just x  -> Data.Text.concat ["metafield[value]=", pack $ show x]
    , case value_type of
        Nothing -> ""
        Just x  -> Data.Text.concat ["metafield[value_type]=", pack $ show x]
    , case updated_at of
        Nothing -> ""
        Just x  -> Data.Text.concat ["metafield[updated_at]=", pack $ show x]
    ]

instance HasCreatedAt Metafield where
  theCreatedAtMaybe = created_at     
  setCreatedAt x y = y { created_at = Just x }

instance HasDescription Metafield where
  theDescriptionMaybe = description    
  setDescription x y = y { description = Just x }

instance HasID Metafield where
  theIDMaybe = id             
  setID x y = y { id = Just x }

instance HasKey Metafield where
  theKeyMaybe = key            
  setKey x y = y { key = Just x }

instance HasNamespace Metafield where
  theNamespaceMaybe = namespace      
  setNamespace x y = y { namespace = Just x }

instance HasOwnerID Metafield where
  theOwnerIDMaybe = owner_id       
  setOwnerID x y = y { owner_id = Just x }

instance HasOwnerResource Metafield where
  theOwnerResourceMaybe = owner_resource 
  setOwnerResource x y = y { owner_resource = Just x }

instance HasValue Metafield where
  theValueMaybe = value          
  setValue x y = y { value = Just x }

instance HasValueType Metafield where
  theValueTypeMaybe = value_type     
  setValueType x y = y { value_type = Just x }

instance HasUpdatedAt Metafield where
  theUpdatedAtMaybe = updated_at     
  setUpdatedAt x y = y { updated_at = Just x }

