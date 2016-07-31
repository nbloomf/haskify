module HaskifyTest (
  allTests
) where

import Haskify

import HaskifyTest.Util
import HaskifyTest.Endpoint

allTests :: HaskifyM TestStatus
allTests = runTests
  [ testMetafield
  , testProduct
  ]
