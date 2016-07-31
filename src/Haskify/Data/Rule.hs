{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Haskify.Data.Rule (
  Rule()
  , HasRule, theRuleMaybe, setRule, theRule, HasRules, theRulesMaybe, setRules, theRules
) where

import Prelude hiding (id, zip)

import Data.Text (Text, pack, concat, intercalate)
import Data.Aeson
import Data.Time.Clock (UTCTime)

import Haskify.Types
import Haskify.DataFields
import Haskify.JSON
import Haskify.HaskifyM
import Haskify.Value.Column
import Haskify.Value.Relation

data Rule = Rule
 { column    :: Maybe Column
 , relation  :: Maybe Relation
 , condition :: Maybe Text
 } deriving (Eq, Show)

class HasRule t where
  theRuleMaybe :: t -> Maybe Rule
  setRule :: Rule -> t -> t

  theRule :: t -> HaskifyM Rule
  theRule x = case theRuleMaybe x of
    Nothing -> fieldDNE "Rule"
    Just y -> return y

class HasRules t where
  theRulesMaybe :: t -> Maybe [Rule]
  setRules :: [Rule] -> t -> t

  theRules :: t -> HaskifyM [Rule]
  theRules x = case theRulesMaybe x of
    Nothing -> fieldDNE "Rule"
    Just y -> return y

instance NullObject Rule where
  nullObject = Rule
    { column    = Nothing
    , relation  = Nothing
    , condition = Nothing
   }

instance FromJSON Rule where
  parseJSON = withObject "Rule" $ \o -> do
    column    <- o .:? "column"
    relation  <- o .:? "relation"
    condition <- o .:? "condition"
    return Rule{..}


instance ToJSON Rule where
  toJSON Rule{..} = objectSansNull
    [ "column"    .= column
    , "relation"  .= relation
    , "condition" .= condition
    ]


instance ToURLArgs Rule where
  toURLArgs Rule{..} = intercalate "&" $ filter (/= "")
    [ case column of
        Nothing -> ""
        Just x  -> Data.Text.concat ["rule[column]=", pack $ show x]
    , case relation of
        Nothing -> ""
        Just x  -> Data.Text.concat ["rule[relation]=", pack $ show x]
    , case condition of
        Nothing -> ""
        Just x  -> Data.Text.concat ["rule[condition]=", pack $ show x]
    ]

instance HasColumn Rule where
  theColumnMaybe = column    
  setColumn x y = y { column = Just x }

instance HasRelation Rule where
  theRelationMaybe = relation  
  setRelation x y = y { relation = Just x }

instance HasCondition Rule where
  theConditionMaybe = condition 
  setCondition x y = y { condition = Just x }

