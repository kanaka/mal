module Mal.Step1 where

import Prelude

import Data.Either (Either(..))
import Effect (Effect)
import Effect.Console (error, log)
import Reader (readStr)
import Printer (printStr)
import Readline (readLine)
import Types (MalExpr)


-- MAIN

main :: Effect Unit
main = loop



-- EVAL

eval :: MalExpr -> MalExpr
eval s = s



-- REPL

rep :: String -> Effect Unit
rep str = case read str of
  Left _  -> error "EOF"
  Right s -> eval s # print >>= log


loop :: Effect Unit
loop = do
  line <- readLine "user> "
  case line of
    ":q" -> pure unit
    ":Q" -> pure unit
    _    -> do
      rep line
      loop



-- READ

read :: String -> Either String MalExpr
read = readStr



-- PRINT

print :: MalExpr -> Effect String
print = printStr