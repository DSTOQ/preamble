module Preamble
  ( module Prelude
  , module DE
  , module DM
  , module TS
  , type (\/)
  , fail
  , notImplemented
  , swapEither
  ) where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Data.Either (Either(..), choose, either, fromLeft, fromRight, hush, isLeft, isRight, note, note') as DE
import Data.Maybe (Maybe(..), fromJust, fromMaybe, fromMaybe', isJust, isNothing, maybe, maybe', optional) as DM
import Effect.Aff (Error, error, throwError)
import Preamble.ToString (class ToString, toString) as TS
import Unsafe.Coerce (unsafeCoerce)

infixr 6 type DE.Either as \/

fail :: ∀ m a . MonadThrow Error m => String -> m a
fail = throwError <<< error

swapEither :: ∀ a b. DE.Either a b -> DE.Either b a
swapEither = DE.either DE.Right DE.Left

notImplemented :: ∀ a . a
notImplemented = unsafeCoerce "Not implemented"
