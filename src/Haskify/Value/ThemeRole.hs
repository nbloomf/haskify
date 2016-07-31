{-# LANGUAGE OverloadedStrings #-}

module Haskify.Value.ThemeRole (ThemeRole(..), HasThemeRole, theThemeRoleMaybe, theThemeRole, setThemeRole) where

import Data.Text (Text)
import Data.Aeson
import Haskify.HaskifyM

data ThemeRole
 = ThemeRoleMain
 | ThemeRoleMobile
 | ThemeRoleUnpublished
 deriving Eq

class HasThemeRole t where
  theThemeRoleMaybe :: t -> Maybe ThemeRole
  setThemeRole :: ThemeRole -> t -> t

  theThemeRole :: t -> HaskifyM ThemeRole
  theThemeRole x = case theThemeRoleMaybe x of
    Nothing -> fieldDNE "ThemeRole"
    Just y -> return y

instance Show ThemeRole where
  show ThemeRoleMain = "main"
  show ThemeRoleMobile = "mobile"
  show ThemeRoleUnpublished = "unpublished"

instance FromJSON ThemeRole where
  parseJSON = withText "ThemeRole" $ \s -> case s of
    "main" -> return ThemeRoleMain
    "mobile" -> return ThemeRoleMobile
    "unpublished" -> return ThemeRoleUnpublished
    _ -> fail "expecting: main mobile unpublished "

instance ToJSON ThemeRole where
  toJSON ThemeRoleMain = String "main"
  toJSON ThemeRoleMobile = String "mobile"
  toJSON ThemeRoleUnpublished = String "unpublished"

