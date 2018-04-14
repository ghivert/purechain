module Main where

import Control.Monad.Eff.Now
import Prelude
import Purechain

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Unsafe
import Partial.Unsafe
import Data.Maybe as Maybe
import Data.Array
import Purechain.Wallet as Wallet
import HelpMe.Console as Console

main :: ∀ e. Eff (console :: CONSOLE, now :: NOW | e) Unit
main = do
  -- Newline for better reading.
  Console.logNewline

  -- Generate wallets.
  firstWallet <- Wallet.newWallet
  secondWallet <- Wallet.newWallet
  log "First wallet:"
  log $ show firstWallet
  Console.logNewline
  log "Second wallet:"
  log $ show secondWallet
  Console.logNewline

  -- Public Key of the first wallet, for better convenience.
  let pubKey = Wallet.publicKey firstWallet

  -- Generate blockchain and add some coins to the first wallet.
  purechain <- genesis pubKey

  let firstWallet' = Wallet.addUTXO firstWallet (unsafePartial $ Maybe.fromJust $ head $ utxo purechain)

  -- Issue a transaction from the first wallet and get it.
  firstWallet <- Wallet.issueTransaction firstWallet' (Wallet.publicKey secondWallet) 2.5
  log $ show firstWallet
  let pendingTransactions = Wallet.pendingTransactions firstWallet

  -- Issue a new transaction by hand.
  -- case Purechain.newTransaction pubKey (Wallet.publicKey secondWallet) 5.0 (utxo purechain) of
  --   Nothing -> log "Not a transaction"
  --   Just trans -> do
  --     transaction <- trans
  --     let updatedTransaction = Purechain.signTransaction (Wallet.privateKey firstWallet) transaction

  -- And finally add it to the next block!
  purechain <- addBlock pubKey pendingTransactions purechain

  Console.logNewline
  log $ show purechain
