{-# LANGUAGE OverloadedStrings #-}

module Haskify.Endpoint.Metafield where

import Haskify.HaskifyM
import Haskify.Data
import Haskify.Singleton
import Haskify.Opt
import Haskify.Types



{- #GetMetafields -}

getMetafields
  :: (GetMetafieldsOpt -> GetMetafieldsOpt) -> HaskifyM [Metafield]
getMetafields f = do
  let path = "/admin/metafields.json"
  let args = toURLArgs $ f emptyOpt
  urlGet path args >>= decodeMetafieldsSingleton



{- #GetMetafieldsMatching -}

getMetafieldsWith
  :: (Metafield -> Metafield) -> HaskifyM [Metafield]
getMetafieldsWith f = do
  let path = "/admin/metafields.json"
  let args = toURLArgs $ f nullObject
  urlGet path args >>= decodeMetafieldsSingleton



{- #GetProductsIDMetafields -}

getProductsIDMetafields
  :: IDNumber -> HaskifyM [Metafield]
getProductsIDMetafields n = do
  let path = "/admin/products/" +$ n $+ "/metafields.json"
  let args = ""
  urlGet path args >>= decodeMetafieldsSingleton



{- #GetMetafieldsCount -}

getMetafieldsCount
  :: HaskifyM Int
getMetafieldsCount = do
  let path = "/admin/metafields/count.json"
  let args = ""
  urlGet path args >>= decodeCountSingleton



{- #GetProductsIDMetafieldsCount -}

getProductsIDMetafieldsCount
  :: IDNumber -> HaskifyM Int
getProductsIDMetafieldsCount n = do
  let path = "/admin/products/" +$ n $+ "/metafields/count.json"
  let args = ""
  urlGet path args >>= decodeCountSingleton



{- #GetMetafieldsID -}

getMetafieldsID
  :: IDNumber -> HaskifyM Metafield
getMetafieldsID n = do
  let path = "/admin/metafields/" +$ n $+ ".json"
  let args = ""
  urlGet path args >>= decodeMetafieldSingleton



{- #GetProductsIDMetafieldsID -}

getProductsIDMetafieldsID
  :: IDNumber -> IDNumber -> HaskifyM Metafield
getProductsIDMetafieldsID n m = do
  let path = "/admin/products/" +$ n $+ "/metafields/" +$ m $+ ".json"
  let args = ""
  urlGet path args >>= decodeMetafieldSingleton



{- #PostMetafields -}

postMetafields
  :: (Metafield -> Metafield) -> HaskifyM Metafield
postMetafields f = do
  let path    = "/admin/metafields.json"
  let args    = ""
  let payload = MetafieldSingleton $ f nullObject
  urlPost path args payload >>= decodeMetafieldSingleton



{- #PostProductsIDMetafields -}

postProductsIDMetafields
  :: IDNumber -> (Metafield -> Metafield) -> HaskifyM Metafield
postProductsIDMetafields n f = do
  let path    = "/admin/products/" +$ n $+ "/metafields.json"
  let args    = ""
  let payload = MetafieldSingleton $ f nullObject
  urlPost path args payload >>= decodeMetafieldSingleton



{- #PutMetafieldsID -}

putMetafieldsID
  :: IDNumber -> (Metafield -> Metafield) -> HaskifyM Metafield
putMetafieldsID n f = do
  let path    = "/admin/metafields/" +$ n $+ ".json"
  let args    = ""
  let payload = MetafieldSingleton $ f nullObject
  urlPut path args payload >>= decodeMetafieldSingleton



{- #PutProductsIDMetafieldsID -}

putProductsIDMetafieldsID
  :: IDNumber -> IDNumber -> (Metafield -> Metafield) -> HaskifyM Metafield
putProductsIDMetafieldsID n m f = do
  let path    = "/admin/products/" +$ n $+ "/metafields/" +$ m $+ ".json"
  let args    = ""
  let payload = MetafieldSingleton $ f nullObject
  urlPut path args payload >>= decodeMetafieldSingleton



{- #DeleteMetafieldsID -}

deleteMetafieldsID
  :: IDNumber -> HaskifyM ()
deleteMetafieldsID n = do
  let path = "/admin/metafields/" +$ n $+ ".json"
  let args = ""
  urlDelete path args >> return ()



{- #DeleteProductsIDMetafieldsID -}

deleteProductsIDMetafieldsID
  :: IDNumber -> IDNumber -> HaskifyM ()
deleteProductsIDMetafieldsID n m = do
  let path = "/admin/products/" +$ n $+ "/metafields/" +$ m $+ ".json"
  let args = ""
  urlDelete path args >> return ()
