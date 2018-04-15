module Main where

import Prelude
import Control.Monad.Eff
import Control.Monad.Eff.Now
import Control.Monad.Eff.Console
import Partial.Unsafe
import Data.Array
import Data.Maybe as Maybe
import Text.Chalky as Colorize

import Purechain
import Purechain.Wallet as Wallet
import HelpMe.Console as Console

logYellow :: forall e. String -> Eff (console :: CONSOLE | e) Unit
logYellow = log <<< Colorize.yellow

logGreen :: forall e. String -> Eff (console :: CONSOLE | e) Unit
logGreen = log <<< Colorize.green

main :: ∀ e. Eff (console :: CONSOLE, now :: NOW | e) Unit
main = do
  -- Newline for better reading.
  Console.logNewline

  -- Generate wallets.
  firstWallet <- Wallet.newWallet
  secondWallet <- Wallet.newWallet
  logYellow "First wallet:"
  logGreen $ show firstWallet
  Console.logNewline
  logYellow "Second wallet:"
  logGreen $ show secondWallet
  Console.logNewline

  -- Public Key of the first wallet, for better convenience.
  let pubKey = Wallet.publicKey firstWallet

  -- Generate blockchain and add some coins to the first wallet.
  purechain <- genesis pubKey

  let firstWallet' = Wallet.addUTXO firstWallet (unsafePartial $ Maybe.fromJust $ head $ utxo purechain)
  logYellow "First wallet after mining genesis:"
  logGreen $ show firstWallet'
  Console.logNewline

  -- Issue a transaction from the first wallet and get it.
  firstWallet <- Wallet.issueTransaction firstWallet' (Wallet.publicKey secondWallet) 2.5
  logYellow "First wallet after issuing transaction:"
  logGreen $ show firstWallet
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
  logGreen $ show purechain
