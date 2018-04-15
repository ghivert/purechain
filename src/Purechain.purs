module Purechain where

import Prelude
import Crypto.Simple as Crypto
import Control.Monad.Eff
import Control.Monad.Eff.Console
import Control.Monad.Eff.Now (NOW)
import Data.List (List(..), singleton, (:))
import Data.Array (concat, difference, union)
import Data.Maybe (Maybe(..), fromJust)
import Data.Timestamp as Timestamp
import Partial.Unsafe (unsafePartial)
import Text.Chalky as Chalk

import Purechain.Block
import Purechain.Transaction (Transaction, inputs, outputs)
import Purechain.Transaction.Output as Transaction
import HelpMe.Buffer (importFromString) as Buffer
import HelpMe.Format

newtype Purechain = Purechain
  { chain :: List Block
  , utxo :: Array Transaction.Output
  , lastState :: Number
  }

blocks :: Purechain -> List Block
blocks (Purechain { chain }) = chain

utxo :: Purechain -> Array Transaction.Output
utxo (Purechain { utxo }) = utxo

lastState :: Purechain -> Number
lastState (Purechain { lastState }) = lastState

instance showPurechain :: Show Purechain where
  show (Purechain { chain, utxo }) =
    "Purechain {\n"
      <> whitepad 2 <> Chalk.red "chain: " <> show chain <> "\n"
      <> whitepad 2 <> Chalk.red "utxo: " <> show utxo <> "\n"
      <> "}"

difficulty :: Int
difficulty = 3

genesis :: ∀ e. Crypto.PublicKey -> Eff (now :: NOW | e) Purechain
genesis miner = do
  genesisBlock <- unsafePartial $ fromJust $ newBlock [] [] (Buffer.importFromString "0")
  timestamp <- Timestamp.now
  let minedBlock = mineBlock difficulty miner genesisBlock
  pure $ Purechain { chain: singleton minedBlock, utxo: [ Transaction.output miner miner 5.0 "0" ], lastState: timestamp }

addBlock :: ∀ e. Crypto.PublicKey -> Array Transaction -> Purechain -> Eff (now :: NOW, console :: CONSOLE | e) Purechain
addBlock miner content (Purechain { chain: Nil }) = genesis miner
addBlock miner content (Purechain purechain @ { chain: (Block hd) : tl, utxo }) =
  case newBlock content utxo $ hd.hash of
    Nothing -> log "test" >>= (\_ -> pure $ Purechain purechain)
    Just block -> do
      notMinedBlock <- block
      timestamp <- Timestamp.now
      let allTransactions = transactions notMinedBlock
      let allInputs = concat $ inputs <$> allTransactions
      let allOutputs = concat $ outputs <$> allTransactions
      pure $ Purechain $ purechain
        { chain = mineBlock difficulty miner notMinedBlock : Block hd : tl
        , lastState = timestamp
        , utxo = union allOutputs $ difference utxo allInputs
        }

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
