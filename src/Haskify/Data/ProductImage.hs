{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Haskify.Data.ProductImage (
  ProductImage()
  , HasProductImage, theProductImageMaybe, setProductImage, theProductImage, HasProductImages, theProductImagesMaybe, setProductImages, theProductImages
) where

import Prelude hiding (id, zip)

import Data.Text (Text, pack, concat, intercalate)
import Data.Aeson
import Data.Time.Clock (UTCTime)

import Haskify.Types
import Haskify.DataFields
import Haskify.JSON
import Haskify.HaskifyM

data ProductImage = ProductImage
 { created_at  :: Maybe DateTime
 , id          :: Maybe IDNumber
 , position    :: Maybe Int
 , product_id  :: Maybe IDNumber
 , variant_ids :: Maybe [IDNumber]
 , src         :: Maybe Text
 , updated_at  :: Maybe DateTime
 } deriving (Eq, Show)

class HasProductImage t where
  theProductImageMaybe :: t -> Maybe ProductImage
  setProductImage :: ProductImage -> t -> t

  theProductImage :: t -> HaskifyM ProductImage
  theProductImage x = case theProductImageMaybe x of
    Nothing -> fieldDNE "ProductImage"
    Just y -> return y

class HasProductImages t where
  theProductImagesMaybe :: t -> Maybe [ProductImage]
  setProductImages :: [ProductImage] -> t -> t

  theProductImages :: t -> HaskifyM [ProductImage]
  theProductImages x = case theProductImagesMaybe x of
    Nothing -> fieldDNE "ProductImage"
    Just y -> return y

instance NullObject ProductImage where
  nullObject = ProductImage
    { created_at  = Nothing
    , id          = Nothing
    , position    = Nothing
    , product_id  = Nothing
    , variant_ids = Nothing
    , src         = Nothing
    , updated_at  = Nothing
   }

instance FromJSON ProductImage where
  parseJSON = withObject "ProductImage" $ \o -> do
    created_at  <- o .:? "created_at"
    id          <- o .:? "id"
    position    <- o .:? "position"
    product_id  <- o .:? "product_id"
    variant_ids <- o .:? "variant_ids"
    src         <- o .:? "src"
    updated_at  <- o .:? "updated_at"
    return ProductImage{..}


instance ToJSON ProductImage where
  toJSON ProductImage{..} = objectSansNull
    [ "created_at"  .= created_at
    , "id"          .= id
    , "position"    .= position
    , "product_id"  .= product_id
    , "variant_ids" .= variant_ids
    , "src"         .= src
    , "updated_at"  .= updated_at
    ]


instance ToURLArgs ProductImage where
  toURLArgs ProductImage{..} = intercalate "&" $ filter (/= "")
    [ case created_at of
        Nothing -> ""
        Just x  -> Data.Text.concat ["product_image[created_at]=", pack $ show x]
    , case id of
        Nothing -> ""
        Just x  -> Data.Text.concat ["product_image[id]=", pack $ show x]
    , case position of
        Nothing -> ""
        Just x  -> Data.Text.concat ["product_image[position]=", pack $ show x]
    , case product_id of
        Nothing -> ""
        Just x  -> Data.Text.concat ["product_image[product_id]=", pack $ show x]
    , case variant_ids of
        Nothing -> ""
        Just x  -> Data.Text.concat ["product_image[variant_ids]=", pack $ show x]
    , case src of
        Nothing -> ""
        Just x  -> Data.Text.concat ["product_image[src]=", pack $ show x]
    , case updated_at of
        Nothing -> ""
        Just x  -> Data.Text.concat ["product_image[updated_at]=", pack $ show x]
    ]

instance HasCreatedAt ProductImage where
  theCreatedAtMaybe = created_at  
  setCreatedAt x y = y { created_at = Just x }

instance HasID ProductImage where
  theIDMaybe = id          
  setID x y = y { id = Just x }

instance HasPosition ProductImage where
  thePositionMaybe = position    
  setPosition x y = y { position = Just x }

instance HasProductID ProductImage where
  theProductIDMaybe = product_id  
  setProductID x y = y { product_id = Just x }

instance HasVariantIDs ProductImage where
  theVariantIDsMaybe = variant_ids 
  setVariantIDs x y = y { variant_ids = Just x }

instance HasSrc ProductImage where
  theSrcMaybe = src         
  setSrc x y = y { src = Just x }

instance HasUpdatedAt ProductImage where
  theUpdatedAtMaybe = updated_at  
  setUpdatedAt x y = y { updated_at = Just x }

