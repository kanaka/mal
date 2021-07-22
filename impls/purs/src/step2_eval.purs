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



-- READ

read :: String -> Either String MalExpr
read = readStr



-- EVAL

eval :: ReplEnv -> MalExpr -> Effect MalExpr
eval _ ast@(MalList _ Nil)  = pure ast
eval env (MalList _ ast)    = do
  es <- traverse (evalAst env) ast
  case es of
    MalFunction {fn:f}: args -> f args
    _                        -> pure $ toList es
eval env ast                = evalAst env ast


evalAst :: ReplEnv -> MalExpr -> Effect MalExpr
evalAst env (MalSymbol s)      = case lookup s env of
  Just f  -> pure f
  Nothing -> throw "invalid function"
evalAst env ast@(MalList _ _ ) = eval env ast
evalAst env (MalVector _ es)   = toVector <$> (traverse (eval env) es)
evalAst env (MalHashMap _ es)  = toHashMap <$> (traverse (eval env) es)
evalAst _ ast                  = pure ast



-- PRINT

print :: MalExpr -> Effect String
print = printStr



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
fn op = MalFunction $ { fn : g op, params:Nil, macro:false, meta:MalNil }
  where
  g :: (Int -> Int -> Int) -> MalFn
  g op' ((MalInt n1) : (MalInt n2) : Nil) = pure $ MalInt $ op' n1 n2
  g _ _                                   = throw "invalid operator"



-- REPL

rep :: String -> Effect Unit
rep str = case read str of
  Left _ -> error "EOF"
  Right ast -> do
    result <- try $ eval replEnv ast
    case result of
      Right exp -> print exp >>= log
      Left err  -> error $ show err


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