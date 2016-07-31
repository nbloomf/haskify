{-# LANGUAGE OverloadedStrings #-}

module Haskify.Value.Commentable (Commentable(..), HasCommentable, theCommentableMaybe, theCommentable, setCommentable) where

import Data.Text (Text)
import Data.Aeson
import Haskify.HaskifyM

data Commentable
 = CommentableNo
 | CommentableModerate
 | CommentableYes
 deriving Eq

class HasCommentable t where
  theCommentableMaybe :: t -> Maybe Commentable
  setCommentable :: Commentable -> t -> t

  theCommentable :: t -> HaskifyM Commentable
  theCommentable x = case theCommentableMaybe x of
    Nothing -> fieldDNE "Commentable"
    Just y -> return y

instance Show Commentable where
  show CommentableNo = "no"
  show CommentableModerate = "moderate"
  show CommentableYes = "yes"

instance FromJSON Commentable where
  parseJSON = withText "Commentable" $ \s -> case s of
    "no" -> return CommentableNo
    "moderate" -> return CommentableModerate
    "yes" -> return CommentableYes
    _ -> fail "expecting: no moderate yes "

instance ToJSON Commentable where
  toJSON CommentableNo = String "no"
  toJSON CommentableModerate = String "moderate"
  toJSON CommentableYes = String "yes"

