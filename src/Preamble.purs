module Preamble
  ( module Prelude
  , module DE
  , module DM
  , type (\/)
  , fail
  , notImplemented
  ) where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Data.Either (Either(..), choose, either, fromLeft, fromRight, hush, isLeft, isRight, note, note') as DE
import Data.Maybe (Maybe(..), fromJust, fromMaybe, fromMaybe', isJust, isNothing, maybe, maybe', optional) as DM
import Effect.Aff (Error, error, throwError)
import Unsafe.Coerce (unsafeCoerce)

infixr 6 type DE.Either as \/

fail :: ∀ m a . MonadThrow Error m => String -> m a
fail = throwError <<< error

notImplemented :: ∀ a . a
notImplemented = unsafeCoerce "Not implemented"
