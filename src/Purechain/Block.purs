module Purechain.Block where

import Control.Monad.Eff
import Control.Monad.Eff.Console
import Control.Monad.Eff.Now
import Control.Monad.Eff.Unsafe
import Data.DateTime.Instant
import Data.Int
import Data.Maybe
import Data.Number.Format
import Data.String
import Data.Time.Duration
import Prelude

import Crypto.Simple as Crypto
import Data.Array as A
import Purechain.Transaction (Transaction, areValid, sendersHaveEnoughFunds)
import Purechain.Transaction.Output as Transaction
import HelpMe.Format

newtype Block = Block
  { hash :: Crypto.Digest
  , previousHash :: Crypto.Digest
  , nonce :: Int
  , content :: Array Transaction
  , timestamp :: Number
  , miner :: Maybe Crypto.PublicKey
  }

hash :: Block -> String
hash (Block { hash }) = Crypto.toString hash

instance showBlock :: Show Block where
  show (Block { hash, previousHash, nonce, content, timestamp, miner }) =
    "Block {\n"
      <> whitepad 2 <> "hash: " <> Crypto.toString hash <> "\n"
      <> whitepad 2 <> "previousHash: " <> Crypto.toString previousHash <> "\n"
      <> whitepad 2 <> "nonce: " <> show nonce <> "\n"
      <> whitepad 2 <> "content: " <> show content <> "\n"
      <> whitepad 2 <> "timestamp: " <> show timestamp <> "\n"
      <> "}"

block :: Crypto.Digest -> Array Transaction -> Number -> Int -> Block
block previousHash content timestamp nonce = Block
  { hash: calculateHash previousHash timestamp nonce content
  , previousHash: previousHash
  , nonce: nonce
  , content: content
  , timestamp: timestamp
  , miner: Nothing
  }

newBlock :: ∀ e
   . Array Transaction
  -> Array Transaction.Output
  -> Crypto.Digest
  -> Maybe (Eff (now :: NOW | e) Block)
newBlock content utxo previousHash =
  if areValid content then
    if sendersHaveEnoughFunds content utxo then Just $ do
      time <- now
      let (Milliseconds timestamp) = unInstant time
      pure $ block previousHash content timestamp 0
    else Nothing
  else Nothing

calculateHash :: Crypto.Digest -> Number -> Int -> Array Transaction -> Crypto.Digest
calculateHash previousHash timestamp nonce content =
  let content' = map show content in
  Crypto.hash Crypto.SHA256
    $ Crypto.toString previousHash
      <> toString timestamp
      <> toStringAs decimal nonce

mineBlock :: Int -> Crypto.PublicKey -> Block -> Block
mineBlock difficulty miner (Block (block @ { hash, nonce, content, timestamp, previousHash })) =
  if checkValidHash difficulty hash then
    unsafePerformEff do
      time <- now
      let (Milliseconds timestamp) = unInstant time
      log $ "Block mined! " <> toString timestamp
      pure $ Block block { miner = Just miner }
  else
    let newNonce = nonce + 1 in
    mineBlock difficulty miner $ Block $ block
      { hash = calculateHash previousHash timestamp newNonce content
      , nonce = newNonce
      }

checkValidHash :: Int -> Crypto.Digest -> Boolean
checkValidHash difficulty hash =
  let target = A.replicate difficulty '0' # fromCharArray in
  take difficulty (Crypto.toString hash) == target
