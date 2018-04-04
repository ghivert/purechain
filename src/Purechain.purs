module Purechain where

import Prelude
import Purechain.Block
import Control.Monad.Eff
import Control.Monad.Eff.Now
import Data.List

newtype Purechain =
  Purechain (List Block)

instance showPurechain :: Show Purechain where
  show (Purechain blocks) = show blocks

genesis :: ∀ e. String -> Eff (now :: NOW | e) Purechain
genesis content = do
  genesisBlock <- newBlock content "0"
  pure $ Purechain $ singleton genesisBlock

addBlock :: ∀ e. String -> Purechain -> Eff (now :: NOW | e) Purechain
addBlock content (Purechain Nil) = genesis content
addBlock content (Purechain ((Block hd) : tl)) = do
  block <- newBlock content hd.hash
  pure $ Purechain $ block : Block hd : tl
