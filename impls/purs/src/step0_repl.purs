module Mal.Step0 where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Readline (readLine)



main :: Effect Unit
main = loop


loop :: Effect Unit
loop = do
  line <- readLine "user> "
  case line of
    ":q" -> pure unit
    ":Q" -> pure unit
    _ -> do
      log line
      loop


read :: String -> String
read s = s


eval :: String -> String
eval s = s


print :: String -> String
print s = s


rep :: String -> String
rep = read >>> eval >>> print
