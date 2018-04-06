module Purechain.Transaction where

import Prelude
import Data.Number.Format as Format
import Data.Maybe
import Data.Time.Duration
import Data.DateTime.Instant
import Control.Monad.Eff
import Control.Monad.Eff.Now as Now
import Crypto.Simple

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
