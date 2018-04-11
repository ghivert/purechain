module Purechain where

import Control.Monad.Eff
import Control.Monad.Eff.Now
import Data.List
import Prelude
import Purechain.Block

import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Crypto.Simple as Crypto
import Data.Maybe as Maybe
import Node.Buffer (fromString) as Node
import Node.Encoding (Encoding(Hex)) as Node
import Partial.Unsafe (unsafePartial)
import Purechain.Transaction.Output as Transaction
import Purechain.Transaction(Transaction)

newtype Purechain = Purechain
  { chain :: List Block
  , utxo :: Array Transaction.Output
  }

instance showPurechain :: Show Purechain where
  show (Purechain { chain }) = show chain

difficulty :: Int
difficulty = 3

genesis :: ∀ e. Array Transaction -> Eff (now :: NOW | e) Purechain
genesis content = do
  genesisBlock <- newBlock content
    $ unsafePartial $ Maybe.fromJust $ Crypto.importFromBuffer
    $ unsafePerformEff $ Node.fromString "0" Node.Hex
  let minedBlock = mineBlock difficulty genesisBlock
  pure $ Purechain $ { chain: singleton minedBlock, utxo: [] }

addBlock :: ∀ e. Array Transaction -> Purechain -> Eff (now :: NOW | e) Purechain
addBlock content (Purechain { chain: Nil }) = genesis content
addBlock content (Purechain purechain @ { chain: ((Block hd) : tl) }) = do
  block <- newBlock content hd.hash
  let minedBlock = mineBlock difficulty block
  pure $ Purechain $ purechain { chain = minedBlock : Block hd : tl }

isValid :: Purechain -> Boolean
isValid (Purechain { chain: Nil }) = true
isValid (Purechain { chain: (hd : Nil) }) = true
isValid (Purechain purechain @ { chain: (first : second : tl) }) =
  let Block { timestamp, content, hash, nonce, previousHash } = first
      Block second = second in
  if Crypto.toString hash == (Crypto.toString $ calculateHash previousHash timestamp nonce content) then
    if Crypto.toString second.hash == Crypto.toString previousHash then
      if checkValidHash difficulty hash then
        isValid $ Purechain $ purechain { chain = tl }
      else
        false
    else
      false
  else
    false
