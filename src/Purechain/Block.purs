module Purechain.Block where

import Prelude
import Data.Int
import Data.String
import Data.Array as A
import Data.Number.Format
import Data.DateTime.Instant
import Data.Time.Duration
import Control.Monad.Eff
import Control.Monad.Eff.Console
import Control.Monad.Eff.Now
import Control.Monad.Eff.Unsafe

import Crypto.Simple as Crypto

newtype Block = Block
  { hash :: Crypto.Digest
  , previousHash :: Crypto.Digest
  , nonce :: Int
  , content :: String
  , timestamp :: Number
  }

instance showBlock :: Show Block where
  show (Block { hash }) = "Hash: " <> Crypto.toString hash

block :: Crypto.Digest -> String -> Number -> Int -> Block
block previousHash content timestamp nonce =
  Block
    { hash: calculateHash previousHash timestamp nonce content
    , previousHash: previousHash
    , nonce: nonce
    , content: content
    , timestamp: timestamp
    }

newBlock :: ∀ e. String -> Crypto.Digest -> Eff (now :: NOW | e) Block
newBlock content previousHash = do
  time <- now
  let (Milliseconds timestamp) = unInstant time
  pure (block previousHash content timestamp 0)

calculateHash :: Crypto.Digest -> Number -> Int -> String -> Crypto.Digest
calculateHash previousHash timestamp nonce content =
  Crypto.hash Crypto.SHA256
    $ Crypto.toString previousHash <> toString timestamp <> toStringAs decimal nonce <> content

mineBlock :: Int -> Block -> Block
mineBlock difficulty (Block (block @ { hash, nonce, content, timestamp, previousHash })) =
  if checkValidHash difficulty hash then
    unsafePerformEff do
      time <- now
      let (Milliseconds timestamp) = unInstant time
      log $ "Block mined! " <> toString timestamp
      pure $ Block block
  else
    let newNonce = nonce + 1 in
    mineBlock difficulty $ Block $ block
      { hash = calculateHash previousHash timestamp newNonce content
      , nonce = newNonce
      }

checkValidHash :: Int -> Crypto.Digest -> Boolean
checkValidHash difficulty hash =
  let target = A.replicate difficulty '0' # fromCharArray in
  take difficulty (Crypto.toString hash) == target
