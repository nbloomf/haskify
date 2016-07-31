{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Haskify.Data.Product (
  Product()
  , HasProduct, theProductMaybe, setProduct, theProduct, HasProducts, theProductsMaybe, setProducts, theProducts
) where

import Prelude hiding (id, zip)

import Data.Text (Text, pack, concat, intercalate)
import Data.Aeson
import Data.Time.Clock (UTCTime)

import Haskify.Types
import Haskify.DataFields
import Haskify.JSON
import Haskify.HaskifyM
import Haskify.Value.PublishedScope
import Haskify.Data.ProductVariant
import Haskify.Data.ProductImage

data Product = Product
 { body_html                     :: Maybe Text
 , created_at                    :: Maybe DateTime
 , handle                        :: Maybe Text
 , id                            :: Maybe IDNumber
 , images                        :: Maybe [ProductImage]
 , options                       :: Maybe [Object]
 , product_type                  :: Maybe Text
 , published_at                  :: Maybe DateTime
 , published_scope               :: Maybe PublishedScope
 , tags                          :: Maybe Text
 , template_suffix               :: Maybe Text
 , title                         :: Maybe Text
 , metafields_global_title       :: Maybe Text
 , metafields_global_description :: Maybe Text
 , updated_at                    :: Maybe DateTime
 , variants                      :: Maybe [ProductVariant]
 , vendor                        :: Maybe Text
 } deriving (Eq, Show)

class HasProduct t where
  theProductMaybe :: t -> Maybe Product
  setProduct :: Product -> t -> t

  theProduct :: t -> HaskifyM Product
  theProduct x = case theProductMaybe x of
    Nothing -> fieldDNE "Product"
    Just y -> return y

class HasProducts t where
  theProductsMaybe :: t -> Maybe [Product]
  setProducts :: [Product] -> t -> t

  theProducts :: t -> HaskifyM [Product]
  theProducts x = case theProductsMaybe x of
    Nothing -> fieldDNE "Product"
    Just y -> return y

instance NullObject Product where
  nullObject = Product
    { body_html                     = Nothing
    , created_at                    = Nothing
    , handle                        = Nothing
    , id                            = Nothing
    , images                        = Nothing
    , options                       = Nothing
    , product_type                  = Nothing
    , published_at                  = Nothing
    , published_scope               = Nothing
    , tags                          = Nothing
    , template_suffix               = Nothing
    , title                         = Nothing
    , metafields_global_title       = Nothing
    , metafields_global_description = Nothing
    , updated_at                    = Nothing
    , variants                      = Nothing
    , vendor                        = Nothing
   }

instance FromJSON Product where
  parseJSON = withObject "Product" $ \o -> do
    body_html                     <- o .:? "body_html"
    created_at                    <- o .:? "created_at"
    handle                        <- o .:? "handle"
    id                            <- o .:? "id"
    images                        <- o .:? "images"
    options                       <- o .:? "options"
    product_type                  <- o .:? "product_type"
    published_at                  <- o .:? "published_at"
    published_scope               <- o .:? "published_scope"
    tags                          <- o .:? "tags"
    template_suffix               <- o .:? "template_suffix"
    title                         <- o .:? "title"
    metafields_global_title       <- o .:? "metafields_global_title"
    metafields_global_description <- o .:? "metafields_global_description"
    updated_at                    <- o .:? "updated_at"
    variants                      <- o .:? "variants"
    vendor                        <- o .:? "vendor"
    return Product{..}


instance ToJSON Product where
  toJSON Product{..} = objectSansNull
    [ "body_html"                     .= body_html
    , "created_at"                    .= created_at
    , "handle"                        .= handle
    , "id"                            .= id
    , "images"                        .= images
    , "options"                       .= options
    , "product_type"                  .= product_type
    , "published_at"                  .= published_at
    , "published_scope"               .= published_scope
    , "tags"                          .= tags
    , "template_suffix"               .= template_suffix
    , "title"                         .= title
    , "metafields_global_title"       .= metafields_global_title
    , "metafields_global_description" .= metafields_global_description
    , "updated_at"                    .= updated_at
    , "variants"                      .= variants
    , "vendor"                        .= vendor
    ]


instance ToURLArgs Product where
  toURLArgs Product{..} = intercalate "&" $ filter (/= "")
    [ case body_html of
        Nothing -> ""
        Just x  -> Data.Text.concat ["product[body_html]=", pack $ show x]
    , case created_at of
        Nothing -> ""
        Just x  -> Data.Text.concat ["product[created_at]=", pack $ show x]
    , case handle of
        Nothing -> ""
        Just x  -> Data.Text.concat ["product[handle]=", pack $ show x]
    , case id of
        Nothing -> ""
        Just x  -> Data.Text.concat ["product[id]=", pack $ show x]
    , case images of
        Nothing -> ""
        Just x  -> Data.Text.concat ["product[images]=", pack $ show x]
    , case options of
        Nothing -> ""
        Just x  -> Data.Text.concat ["product[options]=", pack $ show x]
    , case product_type of
        Nothing -> ""
        Just x  -> Data.Text.concat ["product[product_type]=", pack $ show x]
    , case published_at of
        Nothing -> ""
        Just x  -> Data.Text.concat ["product[published_at]=", pack $ show x]
    , case published_scope of
        Nothing -> ""
        Just x  -> Data.Text.concat ["product[published_scope]=", pack $ show x]
    , case tags of
        Nothing -> ""
        Just x  -> Data.Text.concat ["product[tags]=", pack $ show x]
    , case template_suffix of
        Nothing -> ""
        Just x  -> Data.Text.concat ["product[template_suffix]=", pack $ show x]
    , case title of
        Nothing -> ""
        Just x  -> Data.Text.concat ["product[title]=", pack $ show x]
    , case metafields_global_title of
        Nothing -> ""
        Just x  -> Data.Text.concat ["product[metafields_global_title]=", pack $ show x]
    , case metafields_global_description of
        Nothing -> ""
        Just x  -> Data.Text.concat ["product[metafields_global_description]=", pack $ show x]
    , case updated_at of
        Nothing -> ""
        Just x  -> Data.Text.concat ["product[updated_at]=", pack $ show x]
    , case variants of
        Nothing -> ""
        Just x  -> Data.Text.concat ["product[variants]=", pack $ show x]
    , case vendor of
        Nothing -> ""
        Just x  -> Data.Text.concat ["product[vendor]=", pack $ show x]
    ]

instance HasBodyHTML Product where
  theBodyHTMLMaybe = body_html                     
  setBodyHTML x y = y { body_html = Just x }

instance HasCreatedAt Product where
  theCreatedAtMaybe = created_at                    
  setCreatedAt x y = y { created_at = Just x }

instance HasHandle Product where
  theHandleMaybe = handle                        
  setHandle x y = y { handle = Just x }

instance HasID Product where
  theIDMaybe = id                            
  setID x y = y { id = Just x }

instance HasProductImages Product where
  theProductImagesMaybe = images                        
  setProductImages x y = y { images = Just x }

instance HasOptions Product where
  theOptionsMaybe = options                       
  setOptions x y = y { options = Just x }

instance HasProductType Product where
  theProductTypeMaybe = product_type                  
  setProductType x y = y { product_type = Just x }

instance HasPublishedAt Product where
  thePublishedAtMaybe = published_at                  
  setPublishedAt x y = y { published_at = Just x }

instance HasPublishedScope Product where
  thePublishedScopeMaybe = published_scope               
  setPublishedScope x y = y { published_scope = Just x }

instance HasTags Product where
  theTagsMaybe = tags                          
  setTags x y = y { tags = Just x }

instance HasTemplateSuffix Product where
  theTemplateSuffixMaybe = template_suffix               
  setTemplateSuffix x y = y { template_suffix = Just x }

instance HasTitle Product where
  theTitleMaybe = title                         
  setTitle x y = y { title = Just x }

instance HasMetafieldsGlobalTitle Product where
  theMetafieldsGlobalTitleMaybe = metafields_global_title       
  setMetafieldsGlobalTitle x y = y { metafields_global_title = Just x }

instance HasMetafieldsGlobalDescription Product where
  theMetafieldsGlobalDescriptionMaybe = metafields_global_description 
  setMetafieldsGlobalDescription x y = y { metafields_global_description = Just x }

instance HasUpdatedAt Product where
  theUpdatedAtMaybe = updated_at                    
  setUpdatedAt x y = y { updated_at = Just x }

instance HasProductVariants Product where
  theProductVariantsMaybe = variants                      
  setProductVariants x y = y { variants = Just x }

instance HasVendor Product where
  theVendorMaybe = vendor                        
  setVendor x y = y { vendor = Just x }

