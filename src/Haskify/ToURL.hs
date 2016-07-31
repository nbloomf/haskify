module Haskify.ToURL where

import Data.Text

class ToURL t where
  toURL :: t -> Text
