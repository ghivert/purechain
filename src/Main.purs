module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Now
import Purechain as P

main :: forall e. Eff (console :: CONSOLE, now :: NOW | e) Unit
main = do
  purechain <- P.genesis "Hi im the first block"
  purechain <- P.addBlock "Yo im the second block" purechain
  purechain <- P.addBlock "Hey im the third block" purechain
  log $ show purechain
  log $ show $ P.isValid purechain

  -- log $ "Hash for block 1 : " <> genesisBlock.hash
  -- log $ "Hash for block 2 : " <> secondBlock.hash
  -- log $ "Hash for block 3 : " <> thirdBlock.hash
