module Purechain where

import Prelude
import Purechain.Block
import Control.Monad.Eff
import Control.Monad.Eff.Now
import Data.List

newtype Purechain = Purechain (List Block)

instance showPurechain :: Show Purechain where
  show (Purechain blocks) = show blocks

difficulty :: Int
difficulty = 5

genesis :: ∀ e. String -> Eff (now :: NOW | e) Purechain
genesis content = do
  genesisBlock <- newBlock content "0"
  let minedBlock = mineBlock difficulty genesisBlock
  pure $ Purechain $ singleton minedBlock

addBlock :: ∀ e. String -> Purechain -> Eff (now :: NOW | e) Purechain
addBlock content (Purechain Nil) = genesis content
addBlock content (Purechain ((Block hd) : tl)) = do
  block <- newBlock content hd.hash
  let minedBlock = mineBlock difficulty block
  pure $ Purechain $ minedBlock : Block hd : tl

isValid :: Purechain -> Boolean
isValid (Purechain Nil) = true
isValid (Purechain (hd : Nil)) = true
isValid (Purechain (first : second : tl)) =
  let Block { timestamp, content, hash, nonce, previousHash } = first
      Block second = second in
  if hash == calculateHash previousHash timestamp nonce content then
    if second.hash == previousHash then
      if checkValidHash difficulty hash then
        isValid $ Purechain tl
      else
        false
    else
      false
  else
    false
