module HelpMe.Console where

import Prelude
import Control.Monad.Eff
import Control.Monad.Eff.Console

logNewline :: ∀ e. Eff (console :: CONSOLE | e) Unit
logNewline = log "\n"
