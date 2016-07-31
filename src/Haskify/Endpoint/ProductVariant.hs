{-# LANGUAGE OverloadedStrings #-}

module Haskify.Endpoint.ProductVariant where

import Haskify.HaskifyM
import Haskify.Data
import Haskify.Singleton
import Haskify.Opt
import Haskify.Types



{- #GetProductsIDVariants -}

getProductsIDVariants
  :: IDNumber -> (GetProductsIDVariantsOpt -> GetProductsIDVariantsOpt)
      -> HaskifyM [ProductVariant]
getProductsIDVariants n f = do
  let path = "/admin/products/" +$ n $+ "/variants.json"
  let args = toURLArgs $ f emptyOpt
  urlGet path args >>= decodeVariantsSingleton



{- #GetProductsIDVariantsCount -}

getProductsIDVariantsCount
  :: IDNumber -> HaskifyM Int
getProductsIDVariantsCount n = do
  let path = "/admin/products/" +$ n $+ "/variants/count.json"
  let args = ""
  urlGet path args >>= decodeCountSingleton
