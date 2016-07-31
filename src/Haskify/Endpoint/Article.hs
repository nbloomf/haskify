{-# LANGUAGE OverloadedStrings #-}

module Haskify.Endpoint.Article where

import Haskify.HaskifyM
import Haskify.Data
import Haskify.Singleton
import Haskify.Opt
import Haskify.Types

import Data.Text



{- #GetBlogsIDArticles -}

getBlogsIDArticles
  :: IDNumber -> (GetBlogsIDArticlesOpt -> GetBlogsIDArticlesOpt) -> HaskifyM [Article]
getBlogsIDArticles n f = do
  let path = "/admin/blogs/" +$ n $+ "/articles.json"
  let args = toURLArgs $ f emptyOpt
  urlGet path args >>= decodeArticlesSingleton



{- #GetBlogsIDArticlesCount -}

getBlogsIDArticlesCount
  :: IDNumber -> (GetBlogsIDArticlesCountOpt -> GetBlogsIDArticlesCountOpt) -> HaskifyM Int
getBlogsIDArticlesCount n f = do
  let path = "/admin/blogs/" +$ n $+ "/articles/count.json"
  let args = toURLArgs $ f emptyOpt
  urlGet path args >>= decodeCountSingleton



{- #GetBlogsIDArticlesID -}

getBlogsIDArticlesID
  :: IDNumber -> IDNumber -> HaskifyM Article
getBlogsIDArticlesID n m = do
  let path = "/articles/blogs/" +$ n $+ "/articles/" +$ m $+ ".json"
  let args = ""
  urlGet path args >>= decodeArticleSingleton



{- #PostBlogsIDArticles -}

postBlogsIDArticles
  :: IDNumber -> (Article -> Article) -> HaskifyM Article
postBlogsIDArticles n f = do
  let path    = "/admin/blogs/" +$ n $+ "/articles.json"
  let args    = ""
  let payload = ArticleSingleton $ f nullObject
  urlPost path args payload >>= decodeArticleSingleton



{- #PutBlogsIDArticlesID -}

putBlogsIDArticlesID
  :: IDNumber -> IDNumber -> (Article -> Article) -> HaskifyM Article
putBlogsIDArticlesID n m f = do
  let path    = "/admin/blogs/" +$ n $+ "/articles/" +$ m $+ ".json"
  let args    = ""
  let payload = ArticleSingleton $ f nullObject
  urlPut path args payload >>= decodeArticleSingleton



{- #GetArticlesAuthors -}

getArticlesAuthors
  :: HaskifyM [Text]
getArticlesAuthors = do
  let path = "/admin/articles/authors.json"
  let args = ""
  urlGet path args >>= decodeAuthorsSingleton



{- #GetBlogsIDArticlesTags -}

getBlogsIDArticlesTags
  :: IDNumber -> (GetBlogsIDArticlesTagsOpt -> GetBlogsIDArticlesTagsOpt) -> HaskifyM [Text]
getBlogsIDArticlesTags n f = do
  let path = "/admin/blogs/" +$ n $+ "/articles/tags.json"
  let args = toURLArgs $ f emptyOpt
  urlGet path args >>= decodeTagsSingleton



{- #DeleteBlogsIDArticlesID -}

deleteBlogsIDArticlesID
  :: IDNumber -> IDNumber -> HaskifyM ()
deleteBlogsIDArticlesID n m = do
  let path = "/admin/blogs/" +$ n $+ "/articles/" +$ m $+ ".json"
  let args = ""
  urlDelete path args >> return ()
