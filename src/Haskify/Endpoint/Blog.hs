{-# LANGUAGE OverloadedStrings #-}

module Haskify.Endpoint.Blog where

import Haskify.HaskifyM
import Haskify.Data
import Haskify.Singleton
import Haskify.Opt
import Haskify.Types



{- #GetBlogs -}

getBlogs
  :: (GetBlogsOpt -> GetBlogsOpt) -> HaskifyM [Blog]
getBlogs f = do
  let path = "/admin/blogs.json"
  let args = toURLArgs $ f emptyOpt
  urlGet path args >>= decodeBlogsSingleton



{- #GetBlogsCount -}

getBlogsCount
  :: HaskifyM Int
getBlogsCount = do
  let path = "/admin/blogs/count.json"
  let args = ""
  urlGet path args >>= decodeCountSingleton



{- #GetBlogsID -}

getBlogsID
  :: IDNumber -> HaskifyM Blog
getBlogsID n = do
  let path = "/admin/blogs/" +$ n $+ ".json"
  let args = ""
  urlGet path args >>= decodeBlogSingleton



{- #PostBlogs -}

postBlogs
  :: (Blog -> Blog) -> HaskifyM Blog
postBlogs f = do
  let path    = "/admin/blogs.json"
  let args    = ""
  let payload = BlogSingleton $ f nullObject
  urlPost path args payload >>= decodeBlogSingleton



{- #PutBlogsID -}

putBlogsID
  :: IDNumber -> (Blog -> Blog) -> HaskifyM Blog
putBlogsID n f = do
  let path    = "/admin/blogs/" +$ n $+ ".json"
  let args    = ""
  let payload = BlogSingleton $ f nullObject
  urlPut path args payload >>= decodeBlogSingleton



{- #DeleteBlogsID -}

deleteBlogsID
  :: IDNumber -> HaskifyM ()
deleteBlogsID n = do
  let path = "/admin/blogs/" +$ n $+ ".json"
  let args = ""
  urlDelete path args >> return ()
