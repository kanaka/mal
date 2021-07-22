module Mal.Step1 where

import Prelude

import Data.Either (Either(..))
import Effect (Effect)
import Effect.Console (error, log)
import Reader (readStr)
import Printer (printStr)
import Readline (readLine)
import Types (MalExpr)


-- READ

read :: String -> Either String MalExpr
read = readStr



-- EVAL

eval :: MalExpr -> MalExpr
eval s = s



-- PRINT

print :: MalExpr -> Effect String
print = printStr



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



--

main :: Effect Unit
main = loop