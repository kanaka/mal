module Mal.Step4 where

import Prelude

import Control.Monad.Error.Class (try)
import Core as Core
import Data.Either (Either(..))
import Data.List (List(..), foldM, (:))
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
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
  _ <- traverse (setFn re) Core.ns
  _ <- rep re "(def! not (fn* (a) (if a false true)))"
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
    MalList _ (MalSymbol "if" : es)         -> evalIf env es
    MalList _ (MalSymbol "do" : es)         -> evalDo env es
    MalList _ (MalSymbol "fn*" : es)        -> evalFnMatch env es
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


evalIf :: RefEnv -> List MalExpr -> Effect MalExpr
evalIf env (b:t:e:Nil) = do
  cond <- eval env b
  eval env case cond of
    MalNil           -> e
    MalBoolean false -> e
    _                -> t
evalIf env (b:t:Nil)   = do
  cond <- eval env b
  eval env case cond of
    MalNil           -> MalNil
    MalBoolean false -> MalNil
    _                -> t
evalIf _ _             = throw "invalid if"


evalDo :: RefEnv -> List MalExpr -> Effect MalExpr
evalDo env es = foldM (const $ eval env) MalNil es


evalFnMatch :: RefEnv -> List MalExpr -> Effect MalExpr
evalFnMatch env (MalList _ params : body : Nil)   = evalFn env params body
evalFnMatch env (MalVector _ params : body : Nil) = evalFn env params body
evalFnMatch _ _                                   = throw "invalid fn*"


evalFn :: RefEnv -> List MalExpr -> MalExpr -> Effect MalExpr
evalFn env params body = do
  paramsStr <- traverse unwrapSymbol params
  pure $ MalFunction { fn     : fn paramsStr body
                     , ast    : body
                     , env    : env
                     , params : paramsStr
                     , macro  : false
                     , meta   : MalNil
                     }
  where

  fn :: List String -> MalExpr -> MalFn
  fn params' body' = \args -> do
    fnEnv <- Env.newEnv env
    ok <- Env.sets fnEnv params' args
    if ok
      then eval fnEnv body'
      else throw "actual parameters do not match signature "

  unwrapSymbol :: MalExpr -> Effect String
  unwrapSymbol (MalSymbol s) = pure s
  unwrapSymbol _             = throw "fn* parameter must be symbols"



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


setFn :: RefEnv -> Tuple String MalFn -> Effect Unit
setFn env (Tuple sym f) = do
  newEnv <- Env.newEnv Nil
  Env.set env sym $ MalFunction
                      { fn     : f
                      , ast    : MalNil
                      , env    : newEnv
                      , params : Nil
                      , macro  : false
                      , meta   : MalNil
                      }



-- READ

read :: String -> Effect MalExpr
read = readStr



-- PRINT

print :: MalExpr -> Effect String
print = printStr