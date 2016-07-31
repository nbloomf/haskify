{-# LANGUAGE OverloadedStrings #-}

module Haskify.Endpoint.Product where

import Haskify.HaskifyM
import Haskify.Data
import Haskify.Singleton
import Haskify.Opt
import Haskify.Types



{- #GetProducts -}

getProducts
  :: (GetProductsOpt -> GetProductsOpt) -> HaskifyM [Product]
getProducts f = do
  let path = "/admin/products.json"
  let args = toURLArgs $ f emptyOpt
  urlGet path args >>= decodeProductsSingleton



{- #GetProductsCount -}

getProductsCount
  :: HaskifyM Int
getProductsCount = do
  let path = "/admin/products/count.json"
  let args = ""
  urlGet path args >>= decodeCountSingleton



{- #GetProductsID -}

getProductsID
  :: IDNumber -> (GetProductsIDOpt -> GetProductsIDOpt) -> HaskifyM Product
getProductsID n f = do
  let path = "/admin/products/" +$ n $+ ".json"
  let args = toURLArgs $ f emptyOpt
  urlGet path args >>= decodeProductSingleton



{- #PostProducts -}

postProducts
  :: (Product -> Product) -> HaskifyM Product
postProducts f = do
  let path    = "/admin/products.json"
  let args    = ""
  let payload = ProductSingleton $ f nullObject
  urlPost path args payload >>= decodeProductSingleton



{- #PutProductsID -}

putProductsID
  :: IDNumber -> (Product -> Product) -> HaskifyM Product
putProductsID n f = do
  let path    = "/admin/products/" +$ n $+ ".json"
  let args    = "" 
  let payload = ProductSingleton $ f nullObject
  urlPut path args payload >>= decodeProductSingleton



{- #DeleteProductsID -}

deleteProductsID
  :: IDNumber -> HaskifyM ()
deleteProductsID n = do
  let path = "/admin/products/" +$ n $+ ".json"
  let args = ""
  urlDelete path args >> return ()
