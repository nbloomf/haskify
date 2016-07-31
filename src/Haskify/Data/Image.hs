{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Haskify.Data.Image (
  Image()
  , HasImage, theImageMaybe, setImage, theImage, HasImages, theImagesMaybe, setImages, theImages
) where

import Prelude hiding (id, zip)

import Data.Text (Text, pack, concat, intercalate)
import Data.Aeson
import Data.Time.Clock (UTCTime)

import Haskify.Types
import Haskify.DataFields
import Haskify.JSON
import Haskify.HaskifyM

data Image = Image
 { src        :: Maybe Text
 , attachment :: Maybe Text
 , created_at :: Maybe DateTime
 } deriving (Eq, Show)

class HasImage t where
  theImageMaybe :: t -> Maybe Image
  setImage :: Image -> t -> t

  theImage :: t -> HaskifyM Image
  theImage x = case theImageMaybe x of
    Nothing -> fieldDNE "Image"
    Just y -> return y

class HasImages t where
  theImagesMaybe :: t -> Maybe [Image]
  setImages :: [Image] -> t -> t

  theImages :: t -> HaskifyM [Image]
  theImages x = case theImagesMaybe x of
    Nothing -> fieldDNE "Image"
    Just y -> return y

instance NullObject Image where
  nullObject = Image
    { src        = Nothing
    , attachment = Nothing
    , created_at = Nothing
   }

instance FromJSON Image where
  parseJSON = withObject "Image" $ \o -> do
    src        <- o .:? "src"
    attachment <- o .:? "attachment"
    created_at <- o .:? "created_at"
    return Image{..}


instance ToJSON Image where
  toJSON Image{..} = objectSansNull
    [ "src"        .= src
    , "attachment" .= attachment
    , "created_at" .= created_at
    ]


instance ToURLArgs Image where
  toURLArgs Image{..} = intercalate "&" $ filter (/= "")
    [ case src of
        Nothing -> ""
        Just x  -> Data.Text.concat ["image[src]=", pack $ show x]
    , case attachment of
        Nothing -> ""
        Just x  -> Data.Text.concat ["image[attachment]=", pack $ show x]
    , case created_at of
        Nothing -> ""
        Just x  -> Data.Text.concat ["image[created_at]=", pack $ show x]
    ]

instance HasSrc Image where
  theSrcMaybe = src        
  setSrc x y = y { src = Just x }

instance HasAttachment Image where
  theAttachmentMaybe = attachment 
  setAttachment x y = y { attachment = Just x }

instance HasCreatedAt Image where
  theCreatedAtMaybe = created_at 
  setCreatedAt x y = y { created_at = Just x }

