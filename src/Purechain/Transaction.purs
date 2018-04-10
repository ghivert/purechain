module Purechain.Transaction where

import Control.Monad.Eff
import Crypto.Simple
import Data.DateTime.Instant
import Data.Maybe
import Data.Time.Duration
import Prelude
import Data.String as String
import Control.Monad.Eff.Now as Now
import Data.Maybe as Maybe
import Data.Number.Format as Format
import Node.Buffer as Node
import Node.Encoding as Node
import Partial.Unsafe (unsafePartial)
import Purechain.Transaction.Input as Input
import Purechain.Transaction.Output as Output

newtype Transaction = Transaction
  { id :: String
  , sender :: PublicKey
  , recipient :: PublicKey
  , value :: Number
  , signature :: Maybe Signature
  , inputs :: Array Input.Input
  , outputs :: Array Output.Output
  }

newTransaction :: forall e
   . PublicKey
  -> PublicKey
  -> Number
  -> Array Input.Input
  -> Eff (now :: Now.NOW | e ) Transaction
newTransaction from to value inputs = do
  transactionHash <- calculateHash from to value
  pure $ Transaction
    { id: transactionHash
    , sender: from
    , recipient: to
    , value: value
    , signature: Nothing
    , inputs: inputs
    , outputs: []
    }

transactionDigest :: Transaction -> Digest
transactionDigest (Transaction { sender, recipient, inputs }) =
  hash SHA256 $ toString sender <> toString recipient <> (String.joinWith "" $ map show inputs)

signTransaction :: PrivateKey -> Transaction -> Transaction
signTransaction privateKey transaction @ (Transaction content) =
  Transaction $ content { signature = sign privateKey $ transactionDigest transaction }

isValid :: Transaction -> Boolean
isValid transaction @ (Transaction { signature, sender }) =
  case signature of
    Nothing -> false
    Just sig -> verify sender sig $ transactionDigest transaction

calculateHash :: forall e
   . PublicKey
  -> PublicKey
  -> Number
  -> Eff (now :: Now.NOW | e) String
calculateHash sender recipient value = do
  time <- Now.now
  let (Milliseconds timestamp) = unInstant time
  pure $ toString
       $ hash SHA256
       $ toString sender
         <> toString recipient
         <> Format.toString value
         <> Format.toString timestamp
