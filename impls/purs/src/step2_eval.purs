module Mal.Step2 where

import Prelude

import Data.Either (Either(..))
import Data.List (List(..), (:))
import Data.Map (Map, lookup)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (error, log)
import Effect.Exception (throw, try)
import Reader (readStr)
import Printer (printStr)
import Readline (readLine)
import Types (MalExpr(..), MalFn, toHashMap, toList, toVector)


-- MAIN

main :: Effect Unit
main = loop



-- EVAL

eval :: MalExpr -> Effect MalExpr
eval ast@(MalList _ Nil) = pure ast
eval (MalList _ ast)     = do
  es <- traverse evalAst ast
  case es of
    MalFunction {fn:f}: args -> f args
    _                        -> pure $ toList es
eval ast                 = evalAst ast


evalAst :: MalExpr -> Effect MalExpr
evalAst (MalSymbol s)      = case lookup s replEnv of
  Just f  -> pure f
  Nothing -> throw "invalid function"
evalAst ast@(MalList _ _ ) = eval ast
evalAst (MalVector _ es)   = toVector <$> (traverse eval es)
evalAst (MalHashMap _ es)  = toHashMap <$> (traverse eval es)
evalAst ast                = pure ast



-- ENV

type ReplEnv = Map String MalExpr

replEnv :: ReplEnv
replEnv = Map.fromFoldable
  [ (Tuple "+" (fn (+)))
  , (Tuple "-" (fn (-)))
  , (Tuple "*" (fn (*)))
  , (Tuple "/" (fn (/)))
  ]

fn :: (Int -> Int -> Int) -> MalExpr
fn op =
  MalFunction
    { fn     : g op
    , ast    : MalNil
    , env    : Nil
    , params : Nil
    , macro  : false
    , meta   : MalNil
    }
  where
  g :: (Int -> Int -> Int) -> MalFn
  g op' ((MalInt n1) : (MalInt n2) : Nil) = pure $ MalInt $ op' n1 n2
  g _ _                                   = throw "invalid operator"



-- REPL

rep :: String -> Effect Unit
rep str = do
  result <- try $ eval =<< read str
  case result of
    Left err  -> error $ show err
    Right exp -> print exp >>= log


loop :: Effect Unit
loop = do
  line <- readLine "user> "
  case line of
    ""   -> loop
    ":q" -> pure unit
    _    -> rep line *> loop



-- READ

read :: String -> Effect MalExpr
read = readStr



-- PRINT

print :: MalExpr -> Effect String
print = printStr