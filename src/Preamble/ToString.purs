module Preamble.ToString where

import Prelude

import Data.Maybe (Maybe, maybe)

class ToString a where
  toString :: a -> String

instance toStringString :: ToString String where
  toString = identity

instance toStringMaybe :: ToString a => ToString (Maybe a) where
  toString = maybe "" toString
