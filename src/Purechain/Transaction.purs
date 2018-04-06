module Purechain.Transaction where

import Prelude
import Crypto.Simple
import Data.Maybe

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

newTransaction ::
     PublicKey
  -> PublicKey
  -> Number
  -> Array Input.Input
  -> Transaction
newTransaction from to value inputs =
  Transaction
    { id: ""
    , sender: from
    , recipient: to
    , value: value
    , signature: Nothing
    , inputs: inputs
    , outputs: []
    }
