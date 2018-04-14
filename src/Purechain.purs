module Purechain where

import Control.Monad.Eff
import Control.Monad.Eff.Now
import Data.List
import Prelude
import Purechain.Block
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Crypto.Simple as Crypto
import Data.Maybe (Maybe(..), fromJust)
import HelpMe.Buffer (importFromString) as Buffer
import Node.Buffer (fromString) as Node
import Node.Encoding (Encoding(Hex)) as Node
import Partial.Unsafe (unsafePartial)
import Purechain.Transaction (Transaction)
import Purechain.Transaction.Output as Transaction

newtype Purechain = Purechain
  { chain :: List Block
  , utxo :: Array Transaction.Output
  }

utxo :: Purechain -> Array Transaction.Output
utxo (Purechain { utxo }) = utxo

instance showPurechain :: Show Purechain where
  show (Purechain { chain }) = show chain

difficulty :: Int
difficulty = 3

genesis :: ∀ e. Crypto.PublicKey -> Eff (now :: NOW | e) Purechain
genesis miner = do
  genesisBlock <- unsafePartial $ fromJust $ newBlock [] [] (Buffer.importFromString "0")
  let minedBlock = mineBlock difficulty miner genesisBlock
  pure $ Purechain { chain: singleton minedBlock, utxo: [ Transaction.output miner 5.0 "0" ] }

addBlock :: ∀ e. Crypto.PublicKey -> Array Transaction -> Purechain -> Eff (now :: NOW | e) Purechain
addBlock miner content (Purechain { chain: Nil }) = genesis miner
addBlock miner content (Purechain purechain @ { chain: (Block hd) : tl, utxo }) =
  case newBlock content utxo $ hd.hash of
    Nothing -> pure $ Purechain purechain
    Just block -> do
      notMinedBlock <- block
      let minedBlock = mineBlock difficulty miner notMinedBlock
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
