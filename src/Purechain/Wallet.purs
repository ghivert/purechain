module Purechain.Wallet where

import Control.Monad.Eff
import Control.Monad.Eff.Now
import Crypto.Simple
import Data.Maybe
import Prelude

import Data.Array ((:))
import HelpMe.Format (whitepad)
import Purechain.Transaction as Purechain
import Purechain.Transaction.Output as Transaction

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
      <> whitepad 2 <> "priv: " <> toString privateKey <> "\n"
      <> whitepad 2 <> "publ: " <> toString publicKey <> "\n"
      <> whitepad 2 <> "balance: " <> show balance <> "\n"
      <> whitepad 2 <> "utxo: " <> show utxo <> "\n"
      <> whitepad 2 <> "pendingTransactions: " <> show pendingTransactions <> "\n"
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
addUTXO (Wallet wallet @ { utxo }) output = Wallet $ wallet { utxo = output : utxo }

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
