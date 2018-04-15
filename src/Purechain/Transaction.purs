module Purechain.Transaction where

import Prelude
import Crypto.Simple
import Control.Monad.Eff
import Control.Monad.Eff.Now as Now
import Data.DateTime.Instant
import Data.Maybe
import Data.Time.Duration
import Data.Array (foldr)
import Data.Foldable (and)
import Data.Map as Map
import Data.Number.Format as Format
import Data.String as String
import Text.Chalky as Chalk

import Purechain.Transaction.Output as Transaction
import HelpMe.Buffer as HelpMe
import HelpMe.Format

newtype Transaction = Transaction
  { id :: String
  , sender :: PublicKey
  , recipient :: PublicKey
  , value :: Number
  , signature :: Maybe Signature
  , inputs :: Array Transaction.Output
  , outputs :: Array Transaction.Output
  }

inputs :: Transaction -> Array Transaction.Output
inputs (Transaction { inputs }) = inputs

outputs :: Transaction -> Array Transaction.Output
outputs (Transaction { outputs }) = outputs

-- | Creates a new transaction. Wait for sender, receiver,
-- | how much to send and the actual utxo.
newTransaction :: ∀ e
   . PublicKey
  -> PublicKey
  -> Number
  -> Array Transaction.Output
  -> Maybe (Eff (now :: Now.NOW | e ) Transaction)
newTransaction from to value utxo =
  let balance = Transaction.totalBalance from utxo
      result = balance - value in
  if result < 0.0 then Nothing
  else Just $ do
    transactionHash <- calculateHash from to value
    pure $ Transaction
      { id: transactionHash
      , sender: from
      , recipient: to
      , value: value
      , signature: Nothing
      , inputs : utxo
      , outputs: append
          [ Transaction.output from to value transactionHash ]
          (if result == 0.0 then [] else [ Transaction.output from from (balance - value) transactionHash ])
      }

derive instance eqTransaction :: Eq Transaction

instance showTransaction :: Show Transaction where
  show (Transaction { id, sender, recipient, value, signature, inputs, outputs }) =
    "Transaction {\n"
      <> whitepad 2 <> Chalk.red "id: " <> show id <> "\n"
      <> whitepad 2 <> Chalk.red "sender: " <> toString sender <> "\n"
      <> whitepad 2 <> Chalk.red "recipient: " <> toString recipient <> "\n"
      <> whitepad 2 <> Chalk.red "value: " <> show value <> "\n"
      <> whitepad 2 <> Chalk.red "signature: " <> (show $ toString <$> signature) <> "\n"
      <> whitepad 2 <> Chalk.red "inputs: " <> show inputs <> "\n"
      <> whitepad 2 <> Chalk.red "outputs: " <> show outputs <> "\n"
      <> "}"

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
      isBalanceSufficient key = Transaction.totalBalance key utxo >= fromMaybe 0.0 (Map.lookup (toString key) balances) in
    and $ isBalanceSufficient <$> keys

accumulateBalance :: Transaction -> Map.Map String Number -> Map.Map String Number
accumulateBalance (Transaction { value, sender }) map =
  Map.alter (maybe value ((+) value) >>> Just) (toString sender) map

calculateHash :: ∀ e
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
