module Readline where

import Prelude

import Data.List (List, drop, fromFoldable)
import Effect (Effect)



foreign import readLine :: String -> Effect String


foreign import argv :: Array String

args :: List String
args = drop 2 $ fromFoldable argv