{-# LANGUAGE OverloadedStrings #-}

module Haskify.Value.Relation (Relation(..), HasRelation, theRelationMaybe, theRelation, setRelation) where

import Data.Text (Text)
import Data.Aeson
import Haskify.HaskifyM

data Relation
 = RelationGreaterThan
 | RelationLessThan
 | RelationEquals
 | RelationNotEquals
 | RelationStartsWith
 | RelationEndsWith
 | RelationContains
 | RelationNotContains
 deriving Eq

class HasRelation t where
  theRelationMaybe :: t -> Maybe Relation
  setRelation :: Relation -> t -> t

  theRelation :: t -> HaskifyM Relation
  theRelation x = case theRelationMaybe x of
    Nothing -> fieldDNE "Relation"
    Just y -> return y

instance Show Relation where
  show RelationGreaterThan = "greater_than"
  show RelationLessThan = "less_than"
  show RelationEquals = "equals"
  show RelationNotEquals = "not_equals"
  show RelationStartsWith = "starts_with"
  show RelationEndsWith = "ends_with"
  show RelationContains = "contains"
  show RelationNotContains = "not-contains"

instance FromJSON Relation where
  parseJSON = withText "Relation" $ \s -> case s of
    "greater_than" -> return RelationGreaterThan
    "less_than" -> return RelationLessThan
    "equals" -> return RelationEquals
    "not_equals" -> return RelationNotEquals
    "starts_with" -> return RelationStartsWith
    "ends_with" -> return RelationEndsWith
    "contains" -> return RelationContains
    "not-contains" -> return RelationNotContains
    _ -> fail "expecting: greater_than less_than equals not_equals starts_with ends_with contains not-contains "

instance ToJSON Relation where
  toJSON RelationGreaterThan = String "greater_than"
  toJSON RelationLessThan = String "less_than"
  toJSON RelationEquals = String "equals"
  toJSON RelationNotEquals = String "not_equals"
  toJSON RelationStartsWith = String "starts_with"
  toJSON RelationEndsWith = String "ends_with"
  toJSON RelationContains = String "contains"
  toJSON RelationNotContains = String "not-contains"

