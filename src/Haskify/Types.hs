module Haskify.Types where

import Data.Text (Text, pack, append)
import Data.Time (UTCTime)

type IDNumber = Int
type Position = Int
type DateTime = Text

class NullObject t where
  nullObject :: t

{-----------------}
{- #OptionFields -}
{-----------------}

class ToURLArgs t where toURLArgs :: t -> Text

class Opt t where emptyOpt :: t

noOpts :: (Opt t) => t -> t
noOpts = id

plainObject = id

altering = const

(+$) :: (Show a) => Text -> a -> Text
a +$ x = append a (pack $ show x)

($+) :: Text -> Text -> Text
($+) = append


class HasOptCreatedAtMax t where
  optCreatedAtMax :: DateTime -> t -> t

class HasOptCreatedAtMin t where
  optCreatedAtMin :: DateTime -> t -> t

class HasOptLimit t where
  optLimit :: Int -> t -> t

class HasOptPage t where
  optPage :: Int -> t -> t

class HasOptSinceID t where
  optSinceID :: IDNumber -> t -> t

class HasOptUpdatedAtMax t where
  optUpdatedAtMax :: DateTime -> t -> t

class HasOptUpdatedAtMin t where
  optUpdatedAtMin :: DateTime -> t -> t

class HasOptVendor t where
  optVendor :: Text -> t -> t
