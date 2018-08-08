module Preamble
  ( module Prelude
  , fail
  , notImplemented
  ) where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Effect.Aff (Error, error, throwError)
import Unsafe.Coerce (unsafeCoerce)

fail :: ∀ m a . MonadThrow Error m => String -> m a
fail = throwError <<< error

notImplemented :: ∀ a . a
notImplemented = unsafeCoerce "Not implemented"
