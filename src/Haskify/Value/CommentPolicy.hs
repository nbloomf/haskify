{-# LANGUAGE OverloadedStrings #-}

module Haskify.Value.CommentPolicy (CommentPolicy(..), HasCommentPolicy, theCommentPolicy) where

import Data.Text (Text)
import Data.Aeson

data CommentPolicy
 = CommentPolicyNo
 | CommentPolicyModerate
 | CommentPolicyYes
 deriving Eq

class HasCommentPolicy t where
  theCommentPolicy :: t -> CommentPolicy

instance Show CommentPolicy where
  show CommentPolicyNo = "no"
  show CommentPolicyModerate = "moderate"
  show CommentPolicyYes = "yes"

instance FromJSON CommentPolicy where
  parseJSON = withText "CommentPolicy" $ \s -> case s of
    "no" -> return CommentPolicyNo
    "moderate" -> return CommentPolicyModerate
    "yes" -> return CommentPolicyYes
    _ -> fail "expecting: no moderate yes "

instance ToJSON CommentPolicy where
  toJSON CommentPolicyNo = String "no"
  toJSON CommentPolicyModerate = String "moderate"
  toJSON CommentPolicyYes = String "yes"

