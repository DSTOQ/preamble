module Preamble
  ( module Prelude
  , fail
  ) where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Effect.Aff (Error, error, throwError)

fail :: ∀ m a . MonadThrow Error m => String -> m a
fail = throwError <<< error
