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
           | List Bool (List AST) -- List False -> List, List True -> Vector
           | Map (SortedMap String AST)
           | Func (List AST -> MalM AST)

  public export
  Env : Type
  Env = List (IORef (SortedMap String AST))

  public export
  MalM : Type -> Type
  MalM = ReaderT Env (ExceptT AST IO)

export
toString : (readably : Bool) -> AST -> String
toString b (Symbol s) = s
toString b (Str s) =
  case strUncons s of
       Just ('\xff', rest) => strCons ':' rest
       _ => if b then show s else s
toString b (Number x) = show x
toString b (Boolean True) = "true"
toString b (Boolean False) = "false"
toString b Nil = "nil"
toString b (Quasiquote x) = "(quasiquote " ++ toString b x ++ ")"
toString b (Quote x) = "(quote " ++ toString b x ++ ")"
toString b (Unquote x) = "(unquote " ++ toString b x ++ ")"
toString b (SpliceUnquote x) = "(splice-unquote " ++ toString b x ++ ")"
toString b (Deref x) = "(deref " ++ toString b x ++ ")"
toString b (WithMeta x y) = "(with-meta " ++ toString b x ++ " " ++ toString b y ++ ")"
toString b (List False xs) = "(" ++ unwords (map (toString b) xs) ++ ")"
toString b (List True xs) = "[" ++ unwords (map (toString b) xs) ++ "]"
toString b (Map m) = "{" ++ unwords (concatMap (\(x, y) => [toString b (Str x), toString b y]) $ toList m) ++ "}"
toString b (Func f) = "#<function>"

export
implementation Show AST where
  show = toString True

export
implementation Eq AST where
  Symbol x == Symbol y = x == y
  Str x == Str y = x == y
  Number x == Number y = x == y
  Boolean x == Boolean y = x == y
  Nil == Nil = True
  Quasiquote x == Quasiquote y = x == y
  Quote x == Quote y = x == y
  Unquote x == Unquote y = x == y
  SpliceUnquote x == SpliceUnquote y = x == y
  Deref x == Deref y = x == y
  WithMeta a b == WithMeta x y = a == x && b == y
  List _ x == List _ y = x == y
  Map x == Map y = toList x == toList y
  Func _ == Func _ = False
  _ == _ = False
 
export
truthiness : AST -> Bool
truthiness Nil = False
truthiness (Boolean False) = False
truthiness _ = True

export
getEnv : MalM (IORef (SortedMap String AST))
getEnv = do
  env <- ask
  case env of
       [] => throwError $ Str "Internal error: no environment"
       e :: _ => pure e

export
withLocalEnv : MalM a -> MalM a
withLocalEnv x = do
  new <- liftIO $ newIORef empty
  local (new::) x

export
insert : String -> AST -> MalM ()
insert n x = do
  env <- getEnv
  liftIO $ modifyIORef env $ insert n x

export
lookup : String -> MalM AST
lookup n = ask >>= go
  where go : Env -> MalM AST
        go [] = throwError $ Str $ "Symbol " ++ n ++ " not found"
        go (e::es) = do
          val <- map (lookup n) $ liftIO $ readIORef e
          case val of
               Just x => pure x
               Nothing => go es
