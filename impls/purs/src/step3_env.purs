module Mal.Step3 where

import Prelude

import Control.Monad.Error.Class (try)
import Data.Either (Either(..))
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Console (error, log)
import Effect.Exception (throw)
import Env as Env
import Reader (readStr)
import Printer (printStr)
import Readline (readLine)
import Types (MalExpr(..), MalFn, RefEnv, toHashMap, toVector)


-- MAIN

main :: Effect Unit
main = do
  re <- Env.newEnv Nil
  setArithOp re
  loop re



-- EVAL

evalCallFn :: RefEnv -> List MalExpr -> Effect MalExpr
evalCallFn env ast = do
    es <- traverse (eval env) ast
    case es of
      MalFunction {fn:f} : args -> f args
      _                         -> throw "invalid function"


eval :: RefEnv -> MalExpr -> Effect MalExpr
eval env ast = do
  dbgeval <- Env.get env "DEBUG-EVAL"
  case dbgeval of
    Nothing                 -> pure unit
    Just MalNil             -> pure unit
    Just (MalBoolean false) -> pure unit
    _                       -> do
      image <- print ast
      log ("EVAL: " <> image)
  case ast of
    MalSymbol s   -> do
      result <- Env.get env s
      case result of
        Just k  -> pure k
        Nothing -> throw $ "'" <> s <> "'" <> " not found"
    MalList _ (MalSymbol "def!" : es)       -> evalDef env es
    MalList _ (MalSymbol "let*" : es)       -> evalLet env es
    MalList _ es@(_ : _)                    -> evalCallFn env es
    MalVector _ es  -> toVector <$> traverse (eval env) es
    MalHashMap _ es -> toHashMap <$> traverse (eval env) es
    _               -> pure ast


evalDef :: RefEnv -> List MalExpr -> Effect MalExpr
evalDef env (MalSymbol v : e : Nil) = do
  evd <- eval env e
  Env.set env v evd
  pure evd
evalDef _ _                         = throw "invalid def!"


evalLet :: RefEnv -> List MalExpr -> Effect MalExpr
evalLet env (MalList _ ps : e : Nil)   = do
  letEnv <- Env.newEnv env
  letBind letEnv ps
  eval letEnv e
evalLet env (MalVector _ ps : e : Nil) = do
  letEnv <- Env.newEnv env
  letBind letEnv ps
  eval letEnv e
evalLet _ _                            = throw "invalid let*"


letBind :: RefEnv -> List MalExpr -> Effect Unit
letBind _ Nil                       = pure unit
letBind env (MalSymbol ky : e : es) = do
  Env.set env ky =<< eval env e
  letBind env es
letBind _ _                         = throw "invalid let*"



-- REPL

rep :: RefEnv -> String -> Effect String
rep env str = print =<< eval env =<< read str


loop :: RefEnv -> Effect Unit
loop env = do
  line <- readLine "user> "
  case line of
    ""   -> loop env
    ":q" -> pure unit
    _    -> do
      result <- try $ rep env line
      case result of
        Right exp -> log exp
        Left err  -> error $ show err
      loop env


setArithOp :: RefEnv -> Effect Unit
setArithOp env = do
  Env.set env "+" =<< fn (+)
  Env.set env "-" =<< fn (-)
  Env.set env "*" =<< fn (*)
  Env.set env "/" =<< fn (/)


fn :: (Int -> Int -> Int) -> Effect MalExpr
fn op = do
  newEnv <- Env.newEnv Nil
  pure $ MalFunction
          { fn     : g op
          , ast    : MalNil
          , env    : newEnv
          , params : Nil
          , macro  : false
          , meta   : MalNil
          }
  where

  g :: (Int -> Int -> Int) -> MalFn
  g op' ((MalInt n1) : (MalInt n2) : Nil) = pure $ MalInt $ op' n1 n2
  g _ _                                   = throw "invalid operator"



-- READ

read :: String -> Effect MalExpr
read = readStr



-- PRINT

print :: MalExpr -> Effect String
print = printStr