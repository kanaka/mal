module Mal.Step7 where

import Prelude

import Control.Monad.Error.Class (try)
import Control.Monad.Free.Trans (FreeT, runFreeT)
import Control.Monad.Rec.Class (class MonadRec)
import Core as Core
import Data.Either (Either(..))
import Data.Identity (Identity(..))
import Data.List (List(..), foldM, (:))
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse, traverse_)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (error, log)
import Effect.Exception as Ex
import Env as Env
import Printer (printStr)
import Reader (readStr)
import Readline (args, readLine)
import Types (MalExpr(..), MalFn, RefEnv, foldrM, toHashMap, toList, toVector)


-- TYPES

type Eval a = FreeT Identity Effect a


-- MAIN

main :: Effect Unit
main = do
  env <- Env.newEnv Nil
  traverse_ (setFn env) Core.ns
  setFn env $ Tuple "eval" $ setEval env
  rep_ env "(def! not (fn* (a) (if a false true)))"
  rep_ env "(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \"\nnil)\")))))"
  case args of
    Nil               -> do
      Env.set env "*ARGV*" $ toList Nil
      loop env
    script:scriptArgs -> do
      Env.set env "*ARGV*" $ toList $ MalString <$> scriptArgs
      rep_ env $ "(load-file \"" <> script <> "\")"



-- REPL

rep_ :: RefEnv -> String -> Effect Unit
rep_ env str = rep env str *> pure unit


rep :: RefEnv -> String -> Effect String
rep env str = do
  ast <- read str
  result <- runEval $ eval env ast
  print result


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


setEval :: RefEnv -> MalFn
setEval env (ast:Nil) = runEval $ eval env ast
setEval _ _           = throw "illegal call of eval"



-- EVAL

eval :: RefEnv -> MalExpr -> Eval MalExpr
eval env ast = do
  dbgeval <- liftEffect (Env.get env "DEBUG-EVAL")
  liftEffect case dbgeval of
    Nothing                 -> pure unit
    Just MalNil             -> pure unit
    Just (MalBoolean false) -> pure unit
    _                       -> do
      image <- print ast
      log ("EVAL: " <> image)
  case ast of
    MalSymbol s   -> do
      result <- liftEffect $ Env.get env s
      case result of
        Just k  -> pure k
        Nothing -> throw $ "'" <> s <> "'" <> " not found"
    MalList _ (MalSymbol "def!" : es)       -> evalDef env es
    MalList _ (MalSymbol "let*" : es)       -> evalLet env es
    MalList _ (MalSymbol "if" : es)         -> evalIf env es
    MalList _ (MalSymbol "do" : es)         -> evalDo env es
    MalList _ (MalSymbol "fn*" : es)        -> evalFnMatch env es
    MalList _ (MalSymbol "quote" : es)      -> evalQuote env es
    MalList _ (MalSymbol "quasiquote" : es) -> evalQuasiquote env es
    MalList _ es@(_ : _)                    -> evalCallFn env es
    MalVector _ es  -> toVector <$> traverse (eval env) es
    MalHashMap _ es -> toHashMap <$> traverse (eval env) es
    _               -> pure ast



-- Def

evalDef :: RefEnv -> List MalExpr -> Eval MalExpr
evalDef env (MalSymbol v : e : Nil) = do
  evd <- eval env e
  liftEffect $ Env.set env v evd
  pure evd
evalDef _ _                         = throw "invalid def!"



-- Let

evalLet :: RefEnv -> List MalExpr -> Eval MalExpr
evalLet env (MalList _ ps : e : Nil)   = do
  letEnv <- liftEffect $ Env.newEnv env
  letBind letEnv ps
  eval letEnv e
evalLet env (MalVector _ ps : e : Nil) = do
  letEnv <- liftEffect $ Env.newEnv env
  letBind letEnv ps
  eval letEnv e
evalLet _ _                            = throw "invalid let*"


letBind :: RefEnv -> List MalExpr -> Eval Unit
letBind _ Nil                       = pure unit
letBind env (MalSymbol ky : e : es) = do
  ex <- eval env e
  liftEffect $ Env.set env ky ex
  letBind env es
letBind _ _                         = throw "invalid let*"



-- If

evalIf :: RefEnv -> List MalExpr -> Eval MalExpr
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



-- Do

evalDo :: RefEnv -> List MalExpr -> Eval MalExpr
evalDo env es = foldM (const $ eval env) MalNil es



-- Function

evalFnMatch :: RefEnv -> List MalExpr -> Eval MalExpr
evalFnMatch env (MalList _ params : body : Nil)   = evalFn env params body
evalFnMatch env (MalVector _ params : body : Nil) = evalFn env params body
evalFnMatch _ _                                   = throw "invalid fn*"


evalFn :: RefEnv -> List MalExpr -> MalExpr -> Eval MalExpr
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
      then runEval $ eval fnEnv body'
      else throw "actual parameters do not match signature "

  unwrapSymbol :: MalExpr -> Eval String
  unwrapSymbol (MalSymbol s) = pure s
  unwrapSymbol _             = throw "fn* parameter must be symbols"



-- Quote

evalQuote :: RefEnv -> List MalExpr -> Eval MalExpr
evalQuote _ (e:Nil) = pure e
evalQuote _ _       = throw "invalid quote"


evalQuasiquote :: RefEnv -> List MalExpr -> Eval MalExpr
evalQuasiquote env (e:Nil) = eval env =<< quasiquote e
evalQuasiquote  _ _        = throw "invalid quasiquote"


quasiquote :: MalExpr -> Eval MalExpr
quasiquote (MalList _ (MalSymbol "unquote" : x : Nil)) = pure x
quasiquote (MalList _ (MalSymbol "unquote" : _))       = throw "invalid unquote"
quasiquote (MalList _ xs)                              = foldrM qqIter (toList Nil) xs
quasiquote (MalVector _ xs)                            = do
  lst <- foldrM qqIter (toList Nil) xs
  pure $ toList $ MalSymbol "vec" : lst : Nil
quasiquote ast@(MalHashMap _ _)                        = pure $ toList $ MalSymbol "quote" : ast : Nil
quasiquote ast@(MalSymbol _)                           = pure $ toList $ MalSymbol "quote" : ast : Nil
quasiquote ast                                         = pure ast


qqIter :: MalExpr -> MalExpr -> Eval MalExpr
qqIter (MalList _ (MalSymbol "splice-unquote" : x : Nil)) acc = pure $ toList $ MalSymbol "concat" : x : acc : Nil
qqIter (MalList _ (MalSymbol "splice-unquote" : _)) _         = throw "invalid splice-unquote"
qqIter elt acc                                                = do
  qqted <- quasiquote elt
  pure $ toList $ MalSymbol "cons" : qqted : acc : Nil



-- CALL FUNCTION

evalCallFn :: RefEnv -> List MalExpr -> Eval MalExpr
evalCallFn env ast = do
  es <- traverse (eval env) ast
  case es of
    MalFunction {fn:f, ast:MalNil} : args                   -> liftEffect $ f args
    MalFunction {ast:ast', params:params', env:env'} : args -> do
      newEnv <- liftEffect $ Env.newEnv env'
      _ <- liftEffect $ Env.sets newEnv params' args
      eval newEnv ast'
    _                                                       -> throw "invalid function"



-- READ

read :: String -> Effect MalExpr
read = readStr



-- PRINT

print :: MalExpr -> Effect String
print = printStr



-- Utils

runEval :: ∀ m a. MonadRec m => FreeT Identity m a -> m a
runEval = runFreeT $ pure <<< runIdentity


runIdentity :: ∀ a. Identity a -> a
runIdentity (Identity a) = a


throw :: ∀ m a. MonadEffect m => String -> m a
throw = liftEffect <<< Ex.throw