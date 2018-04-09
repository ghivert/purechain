module Purechain where

import Control.Monad.Eff
import Control.Monad.Eff.Now
import Data.List
import Prelude
import Purechain.Block

import Crypto.Simple as Crypto
import Data.Maybe as Maybe
import Node.Buffer (BUFFER, fromString) as Node
import Node.Encoding (Encoding(Hex)) as Node
import Partial.Unsafe (unsafePartial)

newtype Purechain = Purechain (List Block)

instance showPurechain :: Show Purechain where
  show (Purechain blocks) = show blocks

difficulty :: Int
difficulty = 5

genesis :: ∀ e. String -> Eff (now :: NOW, buffer :: Node.BUFFER | e) Purechain
genesis content = do
  buffer <- Node.fromString "0" Node.Hex
  genesisBlock <- newBlock content
    $ unsafePartial
    $ Maybe.fromJust
    $ Crypto.importFromBuffer buffer
  let minedBlock = mineBlock difficulty genesisBlock
  pure $ Purechain $ singleton minedBlock

addBlock :: ∀ e. String -> Purechain -> Eff (now :: NOW, buffer :: Node.BUFFER | e) Purechain
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
  if Crypto.toString hash == (Crypto.toString $ calculateHash previousHash timestamp nonce content) then
    if Crypto.toString second.hash == Crypto.toString previousHash then
      if checkValidHash difficulty hash then
        isValid $ Purechain tl
      else
        false
    else
      false
  else
    false
