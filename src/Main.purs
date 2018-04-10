module Main where

import Control.Monad.Eff.Now
import Prelude
import Purechain.Wallet (newWallet, publicKey, privateKey)

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Purechain as P
import Purechain.Transaction as T

main :: ∀ e. Eff (console :: CONSOLE, now :: NOW | e) Unit
main = do
  -- Generate wallets.
  firstWallet <- newWallet
  secondWallet <- newWallet
  log $ show firstWallet
  log $ show secondWallet

  -- Generate first transaction.
  transaction <- T.newTransaction (publicKey firstWallet) (publicKey secondWallet) 5.0 []
  let updatedTransaction = T.signTransaction (privateKey firstWallet) transaction
  log $ show $ T.isValid updatedTransaction

  -- Generate blockchain.
  purechain <- P.genesis "Hi im the first block"
  purechain <- P.addBlock "Yo im the second block" purechain
  purechain <- P.addBlock "Hey im the third block" purechain
  purechain <- P.addBlock "Yo im the second block" purechain
  purechain <- P.addBlock "Hey im the third block" purechain
  purechain <- P.addBlock "Yo im the second block" purechain
  purechain <- P.addBlock "Hey im the third block" purechain
  purechain <- P.addBlock "Yo im the second block" purechain
  purechain <- P.addBlock "Hey im the third block" purechain
  purechain <- P.addBlock "Yo im the second block" purechain
  purechain <- P.addBlock "Hey im the third block" purechain
  purechain <- P.addBlock "Yo im the second block" purechain
  purechain <- P.addBlock "Hey im the third block" purechain
  purechain <- P.addBlock "Yo im the second block" purechain
  purechain <- P.addBlock "Hey im the third block" purechain
  purechain <- P.addBlock "Yo im the second block" purechain
  purechain <- P.addBlock "Hey im the third block" purechain
  purechain <- P.addBlock "Yo im the second block" purechain
  purechain <- P.addBlock "Hey im the third block" purechain
  log $ show purechain
  log $ show $ P.isValid purechain
