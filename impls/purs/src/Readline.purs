module Readline where

import Prelude

import Effect (Effect)



foreign import readLine :: String -> Effect String