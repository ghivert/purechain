module HelpMe.Buffer where

import Prelude
import Crypto.Simple as Crypto
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Data.Maybe as Maybe
import Node.Buffer (fromString) as Node
import Node.Encoding (Encoding(Hex)) as Node
import Partial.Unsafe (unsafePartial)

importFromString :: forall a. Crypto.Serializable a => String -> a
importFromString str =
  unsafePartial $ Maybe.fromJust $ Crypto.importFromBuffer
    $ unsafePerformEff $ Node.fromString str Node.Hex
