module Purechain.Transaction where

import Control.Monad.Eff
import Crypto.Simple
import Data.DateTime.Instant
import Data.Maybe
import Data.Time.Duration
import Prelude

import Control.Monad.Eff.Now as Now
import Data.Array (foldr)
import Data.Foldable (and)
import Data.Map as Map
import Data.Number.Format as Format
import Data.String as String
import HelpMe.Buffer as HelpMe
import Purechain.Transaction.Output (totalBalance)
import Purechain.Transaction.Output as Transaction

newtype Transaction = Transaction
  { id :: String
  , sender :: PublicKey
  , recipient :: PublicKey
  , value :: Number
  , signature :: Maybe Signature
  , outputs :: Array Transaction.Output
  }

newTransaction :: forall e
   . PublicKey
  -> PublicKey
  -> Number
  -> Array Transaction.Output
  -> Maybe (Eff (now :: Now.NOW | e ) Transaction)
newTransaction from to value utxo =
  let balance = Transaction.totalBalance from utxo in
  if value <= balance then Just $ do
    transactionHash <- calculateHash from to value
    pure $ Transaction
      { id: transactionHash
      , sender: from
      , recipient: to
      , value: value
      , signature: Nothing
      , outputs: []
      }
  else
    Nothing

instance showTransaction :: Show Transaction where
  show (Transaction { id }) = show id

transactionDigest :: Transaction -> Digest
transactionDigest (Transaction { sender, recipient, outputs }) =
  hash SHA256 $ toString sender <> toString recipient <> (String.joinWith "" $ map show outputs)

signTransaction :: PrivateKey -> Transaction -> Transaction
signTransaction privateKey transaction @ (Transaction content) =
  Transaction $ content { signature = sign privateKey $ transactionDigest transaction }

isValid :: Transaction -> Boolean
isValid transaction @ (Transaction { signature, sender }) =
  case signature of
    Nothing -> false
    Just sig -> verify sender sig $ transactionDigest transaction

areValid :: Array Transaction -> Boolean
areValid transactions =
  and $ map isValid transactions

sendersHaveEnoughFunds :: Array Transaction -> Array Transaction.Output -> Boolean
sendersHaveEnoughFunds transactions utxo =
  let balances = foldr accumulateBalance Map.empty transactions
      keys = map HelpMe.importFromString $ Map.keys balances
      isBalanceSufficient key = totalBalance key utxo <= fromMaybe 0.0 (Map.lookup (toString key) balances) in
    and $ map isBalanceSufficient keys

accumulateBalance :: Transaction -> Map.Map String Number -> Map.Map String Number
accumulateBalance (Transaction { value, sender }) map =
  Map.alter (maybe value ((+) value) >>> Just) (toString sender) map

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
