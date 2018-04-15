module HelpMe.Format where

import Prelude
import Data.Array
import Data.String

whitepad :: Int -> String
whitepad padding =
  fromCharArray $ replicate padding ' '
