{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Haskify.Singleton where

import Data.Text
import Data.Aeson
import Data.ByteString.Lazy (ByteString)

import Haskify.Data
import Haskify.HaskifyM
import Haskify.JSON
import Network.HTTP.Types.Header



data ArticleSingleton = ArticleSingleton
 { unArticleSingleton :: Article
 } deriving Show

instance FromJSON ArticleSingleton where
  parseJSON = withObject "article" $ \o -> do
    x <- o .: "article"
    return $ ArticleSingleton x

instance ToJSON ArticleSingleton where
  toJSON ArticleSingleton{..} = objectSansNull
    [ "article" .= unArticleSingleton
    ]

decodeArticleSingleton :: ByteString -> HaskifyM Article
decodeArticleSingleton x = tryDecode x >>= (return . unArticleSingleton)



data ArticlesSingleton = ArticlesSingleton
 { unArticlesSingleton :: [Article]
 } deriving Show

instance FromJSON ArticlesSingleton where
  parseJSON = withObject "articles" $ \o -> do
    x <- o .: "articles"
    return $ ArticlesSingleton x

instance ToJSON ArticlesSingleton where
  toJSON ArticlesSingleton{..} = objectSansNull
    [ "articles" .= unArticlesSingleton
    ]

decodeArticlesSingleton :: ByteString -> HaskifyM [Article]
decodeArticlesSingleton x = tryDecode x >>= (return . unArticlesSingleton)



data AuthorsSingleton = AuthorsSingleton
 { unAuthorsSingleton :: [Text]
 } deriving Show

instance FromJSON AuthorsSingleton where
  parseJSON = withObject "authors" $ \o -> do
    x <- o .: "authors"
    return $ AuthorsSingleton x

instance ToJSON AuthorsSingleton where
  toJSON AuthorsSingleton{..} = objectSansNull
    [ "authors" .= unAuthorsSingleton
    ]

decodeAuthorsSingleton :: ByteString -> HaskifyM [Text]
decodeAuthorsSingleton x = tryDecode x >>= (return . unAuthorsSingleton)



data BlogSingleton = BlogSingleton
 { unBlogSingleton :: Blog
 } deriving Show

instance FromJSON BlogSingleton where
  parseJSON = withObject "blog" $ \o -> do
    x <- o .: "blog"
    return $ BlogSingleton x

instance ToJSON BlogSingleton where
  toJSON BlogSingleton{..} = objectSansNull
    [ "blog" .= unBlogSingleton
    ]

decodeBlogSingleton :: ByteString -> HaskifyM Blog
decodeBlogSingleton x = tryDecode x >>= (return . unBlogSingleton)



data BlogsSingleton = BlogsSingleton
 { unBlogsSingleton :: [Blog]
 } deriving Show

instance FromJSON BlogsSingleton where
  parseJSON = withObject "blogs" $ \o -> do
    x <- o .: "blogs"
    return $ BlogsSingleton x

instance ToJSON BlogsSingleton where
  toJSON BlogsSingleton{..} = objectSansNull
    [ "blogs" .= unBlogsSingleton
    ]

decodeBlogsSingleton :: ByteString -> HaskifyM [Blog]
decodeBlogsSingleton x = tryDecode x >>= (return . unBlogsSingleton)



data CountSingleton = CountSingleton
 { unCountSingleton :: Int
 } deriving Show

instance FromJSON CountSingleton where
  parseJSON = withObject "count" $ \o -> do
    x <- o .: "count"
    return $ CountSingleton x

instance ToJSON CountSingleton where
  toJSON CountSingleton{..} = objectSansNull
    [ "count" .= unCountSingleton
    ]

decodeCountSingleton :: ByteString -> HaskifyM Int
decodeCountSingleton x = tryDecode x >>= (return . unCountSingleton)



data CarrierServiceSingleton = CarrierServiceSingleton
 { unCarrierServiceSingleton :: CarrierService
 } deriving Show

instance FromJSON CarrierServiceSingleton where
  parseJSON = withObject "carrier_service" $ \o -> do
    x <- o .: "carrier_service"
    return $ CarrierServiceSingleton x

instance ToJSON CarrierServiceSingleton where
  toJSON CarrierServiceSingleton{..} = objectSansNull
    [ "carrier_service" .= unCarrierServiceSingleton
    ]

decodeCarrierServiceSingleton :: ByteString -> HaskifyM CarrierService
decodeCarrierServiceSingleton x = tryDecode x >>= (return . unCarrierServiceSingleton)



data CarrierServicesSingleton = CarrierServicesSingleton
 { unCarrierServicesSingleton :: [CarrierService]
 } deriving Show

instance FromJSON CarrierServicesSingleton where
  parseJSON = withObject "carrier_services" $ \o -> do
    x <- o .: "carrier_services"
    return $ CarrierServicesSingleton x

instance ToJSON CarrierServicesSingleton where
  toJSON CarrierServicesSingleton{..} = objectSansNull
    [ "carrier_services" .= unCarrierServicesSingleton
    ]

decodeCarrierServicesSingleton :: ByteString -> HaskifyM [CarrierService]
decodeCarrierServicesSingleton x = tryDecode x >>= (return . unCarrierServicesSingleton)



data CustomCollectionSingleton = CustomCollectionSingleton
 { unCustomCollectionSingleton :: CustomCollection
 } deriving Show

instance FromJSON CustomCollectionSingleton where
  parseJSON = withObject "custom_collection" $ \o -> do
    x <- o .: "custom_collection"
    return $ CustomCollectionSingleton x

instance ToJSON CustomCollectionSingleton where
  toJSON CustomCollectionSingleton{..} = objectSansNull
    [ "custom_collection" .= unCustomCollectionSingleton
    ]

decodeCustomCollectionSingleton :: ByteString -> HaskifyM CustomCollection
decodeCustomCollectionSingleton x = tryDecode x >>= (return . unCustomCollectionSingleton)



data CustomCollectionsSingleton = CustomCollectionsSingleton
 { unCustomCollectionsSingleton :: [CustomCollection]
 } deriving Show

instance FromJSON CustomCollectionsSingleton where
  parseJSON = withObject "custom_collections" $ \o -> do
    x <- o .: "custom_collections"
    return $ CustomCollectionsSingleton x

instance ToJSON CustomCollectionsSingleton where
  toJSON CustomCollectionsSingleton{..} = objectSansNull
    [ "custom_collections" .= unCustomCollectionsSingleton
    ]

decodeCustomCollectionsSingleton :: ByteString -> HaskifyM [CustomCollection]
decodeCustomCollectionsSingleton x = tryDecode x >>= (return . unCustomCollectionsSingleton)



data MetafieldSingleton = MetafieldSingleton
 { unMetafieldSingleton :: Metafield
 } deriving Show

instance FromJSON MetafieldSingleton where
  parseJSON = withObject "metafield" $ \o -> do
    x <- o .: "metafield"
    return $ MetafieldSingleton x

instance ToJSON MetafieldSingleton where
  toJSON MetafieldSingleton{..} = objectSansNull
    [ "metafield" .= unMetafieldSingleton
    ]

decodeMetafieldSingleton :: ByteString -> HaskifyM Metafield
decodeMetafieldSingleton x = tryDecode x >>= (return . unMetafieldSingleton)



data MetafieldsSingleton = MetafieldsSingleton
 { unMetafieldsSingleton :: [Metafield]
 } deriving Show

instance FromJSON MetafieldsSingleton where
  parseJSON = withObject "metafields" $ \o -> do
    x <- o .: "metafields"
    return $ MetafieldsSingleton x

instance ToJSON MetafieldsSingleton where
  toJSON MetafieldsSingleton{..} = objectSansNull
    [ "metafields" .= unMetafieldsSingleton
    ]

decodeMetafieldsSingleton :: ByteString -> HaskifyM [Metafield]
decodeMetafieldsSingleton x = tryDecode x >>= (return . unMetafieldsSingleton)



data ProductSingleton = ProductSingleton
 { unProductSingleton :: Product
 } deriving Show

instance FromJSON ProductSingleton where
  parseJSON = withObject "product" $ \o -> do
    x <- o .: "product"
    return $ ProductSingleton x

instance ToJSON ProductSingleton where
  toJSON ProductSingleton{..} = objectSansNull
    [ "product" .= unProductSingleton
    ]

decodeProductSingleton :: ByteString -> HaskifyM Product
decodeProductSingleton x = tryDecode x >>= (return . unProductSingleton)



data ProductsSingleton = ProductsSingleton
 { unProductsSingleton :: [Product]
 } deriving Show

instance FromJSON ProductsSingleton where
  parseJSON = withObject "products" $ \o -> do
    x <- o .: "products"
    return $ ProductsSingleton x

instance ToJSON ProductsSingleton where
  toJSON ProductsSingleton{..} = objectSansNull
    [ "products" .= unProductsSingleton
    ]

decodeProductsSingleton :: ByteString -> HaskifyM [Product]
decodeProductsSingleton x = tryDecode x >>= (return . unProductsSingleton)



data RateSingleton = RateSingleton
 { unRateSingleton :: CarrierServiceRequest
 } deriving Show

instance FromJSON RateSingleton where
  parseJSON = withObject "rate" $ \o -> do
    x <- o .: "rate"
    return $ RateSingleton x

instance ToJSON RateSingleton where
  toJSON RateSingleton{..} = objectSansNull
    [ "rate" .= unRateSingleton
    ]

decodeRateSingleton :: ByteString -> HaskifyM CarrierServiceRequest
decodeRateSingleton x = tryDecode x >>= (return . unRateSingleton)



data RatesSingleton = RatesSingleton
 { unRatesSingleton :: [CarrierServiceRate]
 } deriving Show

instance FromJSON RatesSingleton where
  parseJSON = withObject "rates" $ \o -> do
    x <- o .: "rates"
    return $ RatesSingleton x

instance ToJSON RatesSingleton where
  toJSON RatesSingleton{..} = objectSansNull
    [ "rates" .= unRatesSingleton
    ]

decodeRatesSingleton :: ByteString -> HaskifyM [CarrierServiceRate]
decodeRatesSingleton x = tryDecode x >>= (return . unRatesSingleton)



data TagsSingleton = TagsSingleton
 { unTagsSingleton :: [Text]
 } deriving Show

instance FromJSON TagsSingleton where
  parseJSON = withObject "tags" $ \o -> do
    x <- o .: "tags"
    return $ TagsSingleton x

instance ToJSON TagsSingleton where
  toJSON TagsSingleton{..} = objectSansNull
    [ "tags" .= unTagsSingleton
    ]

decodeTagsSingleton :: ByteString -> HaskifyM [Text]
decodeTagsSingleton x = tryDecode x >>= (return . unTagsSingleton)



data VariantSingleton = VariantSingleton
 { unVariantSingleton :: ProductVariant
 } deriving Show

instance FromJSON VariantSingleton where
  parseJSON = withObject "variant" $ \o -> do
    x <- o .: "variant"
    return $ VariantSingleton x

instance ToJSON VariantSingleton where
  toJSON VariantSingleton{..} = objectSansNull
    [ "variant" .= unVariantSingleton
    ]

decodeVariantSingleton :: ByteString -> HaskifyM ProductVariant
decodeVariantSingleton x = tryDecode x >>= (return . unVariantSingleton)



data VariantsSingleton = VariantsSingleton
 { unVariantsSingleton :: [ProductVariant]
 } deriving Show

instance FromJSON VariantsSingleton where
  parseJSON = withObject "variants" $ \o -> do
    x <- o .: "variants"
    return $ VariantsSingleton x

instance ToJSON VariantsSingleton where
  toJSON VariantsSingleton{..} = objectSansNull
    [ "variants" .= unVariantsSingleton
    ]

decodeVariantsSingleton :: ByteString -> HaskifyM [ProductVariant]
decodeVariantsSingleton x = tryDecode x >>= (return . unVariantsSingleton)



