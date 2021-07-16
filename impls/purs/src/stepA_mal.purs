module Mal.StepA where

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
import Effect.Exception (message, throw)
import Env as Env
import Reader (readStr)
import Printer (printStr)
import Readline (args, readLine)
import Types (MalExpr(..), MalFn, RefEnv, foldrM, toHashMap, toList, toVector)



main :: Effect Unit
main = do
  let as = args
  env <- Env.newEnv Nil
  traverse (setFn env) Core.ns
    *> setFn env (Tuple "eval" $ setEval env)
    *> rep env "(def! *host-language* \"purescript\")"
    *> rep env "(def! not (fn* (a) (if a false true)))"
    *> rep env "(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \"\nnil)\")))))"
    *> rep env "(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw \"odd number of forms to cond\")) (cons 'cond (rest (rest xs)))))))"
    *> case as of
      Nil         -> do
        Env.set env "*ARGV*" $ toList Nil
        rep env "(println (str \"Mal [\" *host-language* \"]\"))"
        *> loop env
      script:args -> do
        Env.set env "*ARGV*" $ toList $ MalString <$> args
        rep env $ "(load-file \"" <> script <> "\")"
        *> pure unit



-- REPL

rep :: RefEnv -> String -> Effect String
rep env str = case read str of
  Left _    -> throw "EOF"
  Right ast -> print =<< evalAst env ast


loop :: RefEnv -> Effect Unit
loop env = do
  line <- readLine "user> "
  case line of
    ":q" -> pure unit
    _    -> do
      result <- try $ rep env line
      case result of
        Right exp -> log exp
        Left err  -> error $ "Error: " <> message err
      loop env


setFn :: RefEnv -> Tuple String MalFn -> Effect Unit
setFn env (Tuple sym f) =
  Env.set env sym $ MalFunction { fn:f, params:Nil, macro:false, meta:MalNil }


setEval :: RefEnv -> MalFn
setEval env (ast:Nil) = eval env ast
setEval _ _           = throw "illegal call of eval"



-- READ

read :: String -> Either String MalExpr
read = readStr



-- PRINT

print :: MalExpr -> Effect String
print = printStr



-- EVAL

eval :: RefEnv -> MalExpr -> Effect MalExpr
eval _ ast@(MalList _ Nil) = pure ast
eval env (MalList _ ast)   = case ast of
  MalSymbol "def!" : es             -> evalDef env es
  MalSymbol "let*" : es             -> evalLet env es
  MalSymbol "if" : es               -> evalIf env es
  MalSymbol "do" : es               -> evalDo env es
  MalSymbol "fn*" : es              -> evalFnMatch env es

  MalSymbol "quote" : es            -> evalQuote env es
  MalSymbol "quasiquote" : es       -> evalQuasiquote env es
  MalSymbol "quasiquoteexpand" : es -> evalQuasiquoteexpand es

  MalSymbol "defmacro!" : es        -> evalDefmacro env es
  MalSymbol "macroexpand" : es      -> evalMacroexpand env es

  MalSymbol "try*" : es             -> evalTry env es

  _                                 -> do
    es <- traverse (evalAst env) ast
    case es of
      MalFunction {fn:f} : args -> f args
      _                         -> throw "invalid function"
eval env ast               = evalAst env ast


evalAst :: RefEnv -> MalExpr -> Effect MalExpr
evalAst env ast = do
  newAst <- macroexpand env ast
  case newAst of
    MalSymbol s   -> do
      result <- Env.get env s
      case result of
        Just k  -> pure k
        Nothing -> throw $ "'" <> s <> "'" <> " not found"
    l@(MalList _ _ ) -> eval env l
    MalVector _ es   -> toVector <$> traverse (evalAst env) es
    MalHashMap _ es  -> toHashMap <$> traverse (evalAst env) es
    _                -> pure newAst



-- Def

evalDef :: RefEnv -> List MalExpr -> Effect MalExpr
evalDef env (MalSymbol v : e : Nil) = do
  evd <- evalAst env e
  Env.set env v evd
  pure evd
evalDef _ _                         = throw "invalid def!"



-- Let

evalLet :: RefEnv -> List MalExpr -> Effect MalExpr
evalLet env (MalList _ ps : e : Nil)   = do
  letEnv <- Env.newEnv env
  letBind letEnv ps
  evalAst letEnv e
evalLet env (MalVector _ ps : e : Nil) = do
  letEnv <- Env.newEnv env
  letBind letEnv ps
  evalAst letEnv e
evalLet _ _                            = throw "invalid let*"


letBind :: RefEnv -> List MalExpr -> Effect Unit
letBind _ Nil                       = pure unit
letBind env (MalSymbol ky : e : es) = do
  Env.set env ky =<< evalAst env e
  letBind env es
letBind _ _                         = throw "invalid let*"



-- If

evalIf :: RefEnv -> List MalExpr -> Effect MalExpr
evalIf env (b:t:e:Nil) = do
  cond <- evalAst env b
  evalAst env case cond of
    MalNil           -> e
    MalBoolean false -> e
    _                -> t
evalIf env (b:t:Nil)   = do
  cond <- evalAst env b
  evalAst env case cond of
    MalNil           -> MalNil
    MalBoolean false -> MalNil
    _                -> t
evalIf _ _             = throw "invalid if"



-- Do

evalDo :: RefEnv -> List MalExpr -> Effect MalExpr
evalDo env es = foldM (const $ evalAst env) MalNil es




-- Function

evalFnMatch :: RefEnv -> List MalExpr -> Effect MalExpr
evalFnMatch env (MalList _ params : body : Nil)   = evalFn env params body
evalFnMatch env (MalVector _ params : body : Nil) = evalFn env params body
evalFnMatch _ _                                   = throw "invalid fn*"


evalFn :: RefEnv -> List MalExpr -> MalExpr -> Effect MalExpr
evalFn env params body = do
  paramsStr <- traverse unwrapSymbol params
  pure $ MalFunction { fn : fn paramsStr body, params:paramsStr, macro:false, meta:MalNil }
  where

  fn :: List String -> MalExpr -> MalFn
  fn params' body' = \args -> do
    fnEnv <- Env.newEnv env
    ok <- Env.sets fnEnv params' args
    if ok
      then evalAst fnEnv body'
      else throw "actual parameters do not match signature "

  unwrapSymbol :: MalExpr -> Effect String
  unwrapSymbol (MalSymbol s) = pure s
  unwrapSymbol _             = throw "fn* parameter must be symbols"



-- Quote

evalQuote :: RefEnv -> List MalExpr -> Effect MalExpr
evalQuote _ (e:Nil) = pure e
evalQuote _ _       = throw "invalid quote"


evalQuasiquote :: RefEnv -> List MalExpr -> Effect MalExpr
evalQuasiquote env (e:Nil) = evalAst env =<< quasiquote e
evalQuasiquote  _ _        = throw "invalid quasiquote"


evalQuasiquoteexpand :: List MalExpr -> Effect MalExpr
evalQuasiquoteexpand (e:Nil) = quasiquote e
evalQuasiquoteexpand _       = throw "invalid quasiquote"


quasiquote :: MalExpr -> Effect MalExpr
quasiquote (MalList _ (MalSymbol "unquote" : x : Nil)) = pure x
quasiquote (MalList _ (MalSymbol "unquote" : _))       = throw "invalid unquote"
quasiquote (MalList _ xs)                              = foldrM qqIter (toList Nil) xs
quasiquote (MalVector _ xs)                            = do
  lst <- foldrM qqIter (toList Nil) xs
  pure $ toList $ MalSymbol "vec" : lst : Nil
quasiquote ast@(MalHashMap _ _)                        = pure $ toList $ MalSymbol "quote" : ast : Nil
quasiquote ast@(MalSymbol _)                           = pure $ toList $ MalSymbol "quote" : ast : Nil
quasiquote ast                                         = pure ast


qqIter :: MalExpr -> MalExpr -> Effect MalExpr
qqIter (MalList _ (MalSymbol "splice-unquote" : x : Nil)) acc = pure $ toList $ MalSymbol "concat" : x : acc : Nil
qqIter (MalList _ (MalSymbol "splice-unquote" : _)) _         = throw "invalid splice-unquote"
qqIter elt acc                                                = do
  qqted <- quasiquote elt
  pure $ toList $ MalSymbol "cons" : qqted : acc : Nil



-- Macro

evalDefmacro :: RefEnv -> List MalExpr -> Effect MalExpr
evalDefmacro env (MalSymbol a : b : Nil) = do
  f <- evalAst env b
  case f of
    MalFunction fn@{macro:false} -> do
      let m = MalFunction $ fn {macro = true}
      Env.set env a m
      pure m
    _                            -> throw "defmacro! on non-function"
evalDefmacro _ _                         = throw "invalid defmacro!"


evalMacroexpand :: RefEnv -> List MalExpr -> Effect MalExpr
evalMacroexpand env (a:Nil) = macroexpand env a
evalMacroexpand _ _         = throw "invalid macroexpand"


macroexpand :: RefEnv -> MalExpr -> Effect MalExpr
macroexpand env ast@(MalList _ (MalSymbol a : args)) = do
  maybeMacro <- Env.get env a
  case maybeMacro of
    Just (MalFunction {fn:f, macro:true}) -> macroexpand env =<< f args
    _                                     -> pure ast
macroexpand _ ast                                    = pure ast



-- Try

evalTry :: RefEnv -> List MalExpr -> Effect MalExpr
evalTry env (a:Nil) = evalAst env a
evalTry env (thw : MalList _ ((MalSymbol "catch*") : (MalSymbol e) : b : Nil) : Nil) = do
  res <- try $ evalAst env thw
  case res of
    Left err -> do
      tryEnv <- Env.newEnv env
      Env.set tryEnv e $ MalString $ message err -- FIXME:
      evalAst tryEnv b
    Right v -> pure v
evalTry _ _         = throw "invalid try*"
