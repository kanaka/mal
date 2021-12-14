module Mal.Step1 where

import Prelude

import Control.Monad.Error.Class (try)
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Console (error, log)
import Printer (printStr)
import Reader (readStr)
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
rep str = do
  result <- try $ read str
  case result of
    Left err  -> error $ show err
    Right exp -> print (eval exp) >>= log


loop :: Effect Unit
loop = do
  line <- readLine "user> "
  case line of
    ""   -> loop
    ":q" -> pure unit
    _    -> do
      rep line
      loop



-- READ

read :: String -> Effect MalExpr
read = readStr



-- PRINT

print :: MalExpr -> Effect String
print = printStr