module Purechain.Wallet where

import Prelude
import Crypto.Simple
import Control.Monad
import Control.Monad.Eff

newtype Wallet = Wallet
  { privateKey :: PrivateKey
  , publicKey :: PublicKey
  }

derive instance eqWallet :: Eq Wallet
instance showWallet :: Show Wallet where
  show (Wallet { privateKey, publicKey }) =
    "priv: "
      <> toString privateKey
      <> "\npubl: "
      <> toString publicKey

newWallet :: forall e. Eff (e) Wallet
newWallet = do
  { private, public } <- generateKeyPair
  pure $ Wallet { privateKey: private, publicKey: public }
