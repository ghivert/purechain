module Purechain.Wallet where

import Prelude
import Control.Monad.Eff
import Crypto.Simple

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

publicKey :: Wallet -> PublicKey
publicKey (Wallet { publicKey }) = publicKey

privateKey :: Wallet -> PrivateKey
privateKey (Wallet { privateKey }) = privateKey
