module Purechain.Transaction.Input where

import Prelude

newtype Input = Input String

instance showInput :: Show Input where
  show (Input content) = content
