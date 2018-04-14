module Main where

import Control.Monad.Eff.Now
import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Maybe (Maybe(..))
import Purechain as P
import Purechain.Transaction as T
import Purechain.Wallet (newWallet, publicKey, privateKey)

main :: ∀ e. Eff (console :: CONSOLE, now :: NOW | e) Unit
main = do
  -- Generate wallets.
  firstWallet <- newWallet
  secondWallet <- newWallet
  log $ show firstWallet
  log $ show secondWallet

  let pubKey = publicKey firstWallet

  -- Generate blockchain.
  purechain <- P.genesis pubKey

  case T.newTransaction pubKey (publicKey secondWallet) 5.0 (P.utxo purechain) of
    Nothing -> log "Not a transaction"
    Just trans -> do
      transaction <- trans
      let updatedTransaction = T.signTransaction (privateKey firstWallet) transaction
      log $ show $ T.isValid updatedTransaction

  purechain <- P.addBlock pubKey [] purechain
  purechain <- P.addBlock pubKey [] purechain
  purechain <- P.addBlock pubKey [] purechain
  purechain <- P.addBlock pubKey [] purechain
  purechain <- P.addBlock pubKey [] purechain
  purechain <- P.addBlock pubKey [] purechain
  purechain <- P.addBlock pubKey [] purechain
  purechain <- P.addBlock pubKey [] purechain
  purechain <- P.addBlock pubKey [] purechain
  purechain <- P.addBlock pubKey [] purechain
  purechain <- P.addBlock pubKey [] purechain
  purechain <- P.addBlock pubKey [] purechain
  purechain <- P.addBlock pubKey [] purechain
  purechain <- P.addBlock pubKey [] purechain
  purechain <- P.addBlock pubKey [] purechain
  purechain <- P.addBlock pubKey [] purechain
  purechain <- P.addBlock pubKey [] purechain
  purechain <- P.addBlock pubKey [] purechain
  log $ show purechain
  log $ show $ P.isValid purechain
