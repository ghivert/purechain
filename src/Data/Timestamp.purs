module Data.Timestamp where

import Prelude
import Control.Monad.Eff
import Control.Monad.Eff.Now as Now
import Data.DateTime.Instant
import Data.Time.Duration

now :: forall e. Eff (now :: Now.NOW | e) Number
now = do
  time <- Now.now
  let (Milliseconds timestamp) = unInstant time
  pure timestamp
