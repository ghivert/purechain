module Purechain.Wallet where

import Prelude
import Crypto.Simple
import Control.Monad.Eff
import Control.Monad.Eff.Now
import Data.Maybe
import Data.Array
import Text.Chalky as Chalk

import Purechain.Transaction as Purechain
import Purechain.Transaction.Output as Transaction
import HelpMe.Format (whitepad)

newtype Wallet = Wallet
  { privateKey :: PrivateKey
  , publicKey :: PublicKey
  , balance :: Number
  , utxo :: Array Transaction.Output
  , pendingTransactions :: Array Purechain.Transaction
  }

pendingTransactions :: Wallet -> Array Purechain.Transaction
pendingTransactions (Wallet { pendingTransactions }) = pendingTransactions

derive instance eqWallet :: Eq Wallet

instance showWallet :: Show Wallet where
  show (Wallet { privateKey, publicKey, balance, utxo, pendingTransactions }) =
    "Wallet {\n"
      <> whitepad 2 <> Chalk.red "priv: " <> (Chalk.magenta $ toString privateKey) <> "\n"
      <> whitepad 2 <> Chalk.red "publ: " <> (Chalk.magenta $ toString publicKey) <> "\n"
      <> whitepad 2 <> Chalk.red "balance: " <> (Chalk.magenta $ show balance) <> "\n"
      <> whitepad 2 <> Chalk.red "utxo: " <> show utxo <> "\n"
      <> whitepad 2 <> Chalk.red "pendingTransactions: " <> show pendingTransactions <> "\n"
      <>"}"

newWallet :: ∀ e. Eff (e) Wallet
newWallet = do
  { private, public } <- generateKeyPair
  pure $ Wallet
    { privateKey: private
    , publicKey: public
    , balance: 0.0
    , utxo: []
    , pendingTransactions: []
    }

addUTXO :: Wallet -> Transaction.Output -> Wallet
addUTXO (Wallet wallet @ { utxo, balance }) output =
  Wallet $ wallet
    { utxo = output : utxo
    , balance = Transaction.balance output + balance
    }

removeUTXO :: Wallet -> Transaction.Output -> Wallet
removeUTXO (Wallet wallet @ { utxo, balance }) output =
  if elem output utxo then
    Wallet $ wallet
      { utxo = delete output utxo
      , balance = balance - Transaction.balance output
      }
  else
    Wallet wallet

issueTransaction :: ∀ e. Wallet -> PublicKey -> Number -> Eff (now :: NOW | e) Wallet
issueTransaction (Wallet wallet @ { publicKey, privateKey, utxo, pendingTransactions }) receiver value =
  case Purechain.newTransaction publicKey receiver value utxo of
    Nothing -> pure $ Wallet wallet
    Just justTransaction -> do
      transaction <- justTransaction
      let signedTransaction = Purechain.signTransaction privateKey transaction
      pure $ Wallet $ wallet { pendingTransactions = signedTransaction : pendingTransactions }

publicKey :: Wallet -> PublicKey
publicKey (Wallet { publicKey }) = publicKey

privateKey :: Wallet -> PrivateKey
privateKey (Wallet { privateKey }) = privateKey
