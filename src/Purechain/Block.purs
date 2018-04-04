module Purechain.Block where

import Control.Monad.Eff
import Control.Monad.Eff.Now
import Data.DateTime.Instant
import Data.Number.Format
import Data.Time.Duration
import Prelude

import Control.Monad.Eff.Unsafe
import Crypto.Simple as Crypto

newtype Block =
  Block
    { hash :: String
    , previousHash :: String
    , content :: String
    , timestamp :: Number
    }

derive instance eqBlock :: Eq Block
instance showBlock :: Show Block where
  show (Block { hash }) = "Hash: " <> hash

block :: String -> String -> Number -> Block
block previousHash content timestamp =
  Block
    { hash: Crypto.toString
        $ Crypto.hash Crypto.SHA256
        $ previousHash <> toString timestamp <> content
    , previousHash: previousHash
    , content: content
    , timestamp: timestamp
    }

newBlock :: ∀ e. String -> String -> Eff (now :: NOW | e) Block
newBlock content previousHash = do
  time <- now
  let (Milliseconds timestamp) = unInstant time
  pure (block previousHash content timestamp)
