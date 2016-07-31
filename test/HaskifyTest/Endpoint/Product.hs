{-# LANGUAGE OverloadedStrings #-}

module HaskifyTest.Endpoint.Product (
  testProduct
) where

import Haskify

import HaskifyTest.Util

testProduct :: HaskifyM TestStatus
testProduct = testGroup "product"
  [ testGroup "successful requests"
      [ testGetProductsSucceeds
      , testGetProductsCountSucceeds
      ]
  ]



-- get products.json succeeds
testGetProductsSucceeds :: HaskifyM TestStatus
testGetProductsSucceeds = testSuccess
  "testGetProductsSucceeds" (getProducts noOpts)



-- get products/count.json succeeds
testGetProductsCountSucceeds :: HaskifyM TestStatus
testGetProductsCountSucceeds = testSuccess
  "testGetProductsCountSucceeds" getProductsCount
