module Purechain.Transaction.Output where

import Prelude

import Crypto.Simple as Crypto
import Data.Array (filter, foldr)

newtype Output = Output
  { sender :: Crypto.PublicKey
  , receiver :: Crypto.PublicKey
  , balance :: Number
  , txId :: String
  }

balance :: Output -> Number
balance (Output { balance }) = balance

derive instance eqOutput :: Eq Output
instance showOutput :: Show Output where
  show (Output { receiver, balance, txId }) =
    "receiver: " <> Crypto.toString receiver
      <> " balance: " <> show balance
      <> " txId: " <> show txId

output :: Crypto.PublicKey -> Crypto.PublicKey -> Number -> String -> Output
output sender receiver balance txId = Output
  { sender: sender
  , receiver: receiver
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
