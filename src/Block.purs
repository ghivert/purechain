module Block where

import Control.Monad.Eff
import Control.Monad.Eff.Now
import Data.DateTime.Instant
import Prelude

import Crypto.Simple as Crypto
import Data.Time.Duration
import Data.Number.Format

type Block =
  { hash :: String
  , previousHash :: String
  , data_ :: String
  , timestamp :: Number
  }

block :: String -> String -> Number -> Block
block previousHash data_ timestamp =
  { hash: Crypto.toString
      $ hashString
      $ previousHash <> toString timestamp <> data_
  , previousHash: previousHash
  , data_: data_
  , timestamp: timestamp
  }

new :: ∀ e. String -> String -> Eff (now :: NOW | e) Block
new data_ previousHash = do
  time <- now
  let (Milliseconds timestamp) = unInstant time
  pure (block previousHash data_ timestamp)

hashString :: String -> Crypto.Digest
hashString =
  Crypto.hash Crypto.SHA256
