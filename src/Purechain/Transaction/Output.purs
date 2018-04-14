module Purechain.Transaction.Output where

import Prelude

import Crypto.Simple as Crypto
import Data.Array (filter, foldr)

newtype Output = Output
  { receiver :: Crypto.PublicKey
  , balance :: Number
  , txId :: String
  }

balance :: Output -> Number
balance (Output { balance }) = balance

instance showOutput :: Show Output where
  show (Output { receiver, balance, txId }) =
    "receiver: " <> Crypto.toString receiver
      <> " balance: " <> show balance
      <> " txId: " <> show txId

output :: Crypto.PublicKey -> Number -> String -> Output
output receiver balance txId = Output
  { receiver: receiver
  , balance: balance
  , txId: txId
  }

gatherOutputs :: Crypto.PublicKey -> Array Output -> Array Output
gatherOutputs =
  filter <<< isReceiver

totalBalance :: Crypto.PublicKey -> Array Output -> Number
totalBalance publicKey =
  foldr (\output acc -> acc + balance output) 0.0 <<< gatherOutputs publicKey

isReceiver :: Crypto.PublicKey -> Output -> Boolean
isReceiver publicKey (Output { receiver }) = receiver == publicKey
