module Haskify.JSON where

import Data.Text (Text)
import Data.Aeson

objectSansNull :: [(Text,Value)] -> Value
objectSansNull = object . filter (\(_, v) -> v /= Null)
