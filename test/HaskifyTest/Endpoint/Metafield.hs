{-# LANGUAGE OverloadedStrings #-}

module HaskifyTest.Endpoint.Metafield (
  testMetafield
) where

import Haskify

import HaskifyTest.Util


testMetafield :: HaskifyM TestStatus
testMetafield = testGroup "metafield"
  [ testGroup "successful requests"
      [ testGetMetafieldsSucceeds
      , testGetMetafieldsCountSucceeds
      ]
  , testGroup "get"
      [ testGetMetafieldsID
      ]
  , testGroup "post"
      [ testPostDeleteMetafieldCount
      ]
  , testGroup "put"
      [ testPutMetafieldNewValue
      ]
  ]


-- get metafields.json succeeds
testGetMetafieldsSucceeds :: HaskifyM TestStatus
testGetMetafieldsSucceeds = testSuccess
  "testGetMetafieldsSucceeds" (getMetafields noOpts)



-- get metafields/count.json succeeds
testGetMetafieldsCountSucceeds :: HaskifyM TestStatus
testGetMetafieldsCountSucceeds = testSuccess
  "testGetMetafieldsCountSucceeds" getMetafieldsCount



-- 
testGetMetafieldsID :: HaskifyM TestStatus
testGetMetafieldsID = testIsTrue name script
  where
    name = "testGetMetafieldsID"

    script = do
      x <- postMetafields
             ( setNamespace "test"
             . setKey name
             . setValue "value"
             . setValueType ValueTypeString
             )
      n <- theID x
      y <- getMetafieldsID n
      deleteMetafieldsID n
      return (x == y)



-- posting a metafield increases metafield count by 1
-- deleting a metafield decreases metafield count by 1
testPostDeleteMetafieldCount :: HaskifyM TestStatus
testPostDeleteMetafieldCount = testIsTrue name script
  where
    name = "testPostMetafieldIncCount"

    script = do
      n <- getMetafieldsCount
      x <- postMetafields
             ( setNamespace "test"
             . setKey name
             . setValue "value"
             . setValueType ValueTypeString
             )
      m <- getMetafieldsCount
      r <- theID x
      deleteMetafieldsID r
      t <- getMetafieldsCount
      return (n == t && n+1 == m)



-- putting a metafield with a non-null value updates it
testPutMetafieldNewValue :: HaskifyM TestStatus
testPutMetafieldNewValue = testIsTrue name script
  where
    name = "testPutMetafield"

    script = do
      x <- postMetafields
             ( setNamespace "test"
             . setKey name
             . setValue "old"
             . setValueType ValueTypeString
             )
      old <- theValue x
      r   <- theID x
      y <- putMetafieldsID r
             ( setValue "new"
             . altering x
             )
      new <- theValue y
      deleteMetafieldsID r
      return (old == "old" && new == "new")
