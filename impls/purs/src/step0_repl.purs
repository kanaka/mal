module Mal.Step0 where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Readline (readLine)


-- MAIN

main :: Effect Unit
main = loop



-- EVAL

eval :: String -> String
eval s = s



-- REPL

rep :: String -> String
rep = read >>> eval >>> print

loop :: Effect Unit
loop = do
  line <- readLine "user> "
  case line of
    ""   -> loop
    ":q" -> pure unit
    _ -> do
      log line
      loop



-- READ

read :: String -> String
read s = s



-- PRINT

print :: String -> String
print s = s

