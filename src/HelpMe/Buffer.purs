module HelpMe.Buffer where

import Prelude
import Data.Maybe as Maybe
import Partial.Unsafe (unsafePartial)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Node.Buffer (fromString) as Node
import Node.Encoding (Encoding(Hex)) as Node
import Crypto.Simple as Crypto

importFromString :: forall a. Crypto.Serializable a => String -> a
importFromString str =
  unsafePartial $ Maybe.fromJust $ Crypto.importFromBuffer
    $ unsafePerformEff $ Node.fromString str Node.Hex
