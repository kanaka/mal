module Types

import Control.Monad.State
import public Data.IORef
import public Data.SortedMap
import Data.Strings
import public MonadTrans

mutual
  public export
  data AST = Symbol String -- Identifiers
           | Str String -- Includes keywords
           | Number Integer
           | Boolean Bool
           | Nil
           | Quasiquote AST
           | Quote AST
           | Unquote AST
           | SpliceUnquote AST
           | Deref AST
           | WithMeta AST AST
           | List (List AST)
           | Vector (List AST)
           | Map (SortedMap String AST)
           | Func (List AST -> MalM AST)

  public export
  Env : Type
  Env = List (IORef (SortedMap String AST))

  public export
  MalM : Type -> Type
  MalM = ReaderT Env (ExceptT AST IO)

export
implementation Show AST where
  show (Symbol s) = s
  show (Str s) =
    case strUncons s of
         Just ('\xff', rest) => strCons ':' rest
         _ => show s
  show (Number x) = show x
  show (Boolean True) = "true"
  show (Boolean False) = "false"
  show Nil = "nil"
  show (Quasiquote x) = "(quasiquote " ++ show x ++ ")"
  show (Quote x) = "(quote " ++ show x ++ ")"
  show (Unquote x) = "(unquote " ++ show x ++ ")"
  show (SpliceUnquote x) = "(splice-unquote " ++ show x ++ ")"
  show (Deref x) = "(deref " ++ show x ++ ")"
  show (WithMeta a b) = "(with-meta " ++ show a ++ " " ++ show b ++ ")"
  show (List xs) = "(" ++ unwords (map show xs) ++ ")"
  show (Vector xs) = "[" ++ unwords (map show xs) ++ "]"
  show (Map m) = "{" ++ unwords (concatMap (\(a, b) => [show (Str a), show b]) $ toList m) ++ "}"
  show (Func f) = "<function>"

public export
getEnv : MalM (IORef (SortedMap String AST))
getEnv = do
  env <- ask
  case env of
       [] => throwError $ Str "Internal error: no environment"
       e :: _ => pure e

public export
withLocalEnv : MalM a -> MalM a
withLocalEnv x = do
  new <- liftIO $ newIORef empty
  local (new::) x

public export
insert : String -> AST -> MalM ()
insert n x = do
  env <- getEnv
  liftIO $ modifyIORef env $ insert n x

public export
lookup : String -> MalM AST
lookup n = ask >>= go
  where go : Env -> MalM AST
        go [] = throwError $ Str $ "Symbol " ++ n ++ " not found"
        go (e::es) = do
          val <- map (lookup n) $ liftIO $ readIORef e
          case val of
               Just x => pure x
               Nothing => go es
