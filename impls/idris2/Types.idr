module Types

import Control.Monad.State
import public Data.IORef
import Data.List
import public Data.SortedMap
import Data.Strings
import public MonadTrans

mutual
  public export
  data AST = Symbol String -- Identifiers
           | Str String -- Includes keywords
           | Number Integer
           | Atom (IORef AST)
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
toString : (readably : Bool) -> AST -> IO String
toString b (Symbol s) = pure s
toString b (Str s) =
  case strUncons s of
       Just ('\xff', rest) => pure $ strCons ':' rest
       _ => pure $ if b then show s else s
toString b (Number x) = pure $ show x
toString b (Atom a) = do
  x <- readIORef a
  sx <- toString b x
  pure $ "(atom " ++ sx ++ ")"
toString b (WithMeta x y) = do
  sx <- toString b x
  sy <- toString b y
  pure $ "(with-meta " ++ sx ++ " " ++ sy ++ ")"
toString b (List False xs) = do
  sxs <- traverse (toString b) xs
  pure $ "(" ++ unwords sxs ++ ")"
toString b (List True xs) = do
  sxs <- traverse (toString b) xs
  pure $ "[" ++ unwords sxs ++ "]"
toString b (Map m) = do
  sm <- map concat $ traverse onePair $ SortedMap.toList m
  pure $ "{" ++ unwords sm ++ "}"
  where onePair : (String, AST) -> IO (List String)
        onePair (k, v) = sequence [toString b (Str k), toString b v]
toString b (Func f) = pure "#<function>"

export
implementation Eq AST where
  Symbol x == Symbol y = x == y
  Str x == Str y = x == y
  Number x == Number y = x == y
  WithMeta a b == WithMeta x y = a == x && b == y
  List _ x == List _ y = x == y
  Map x == Map y = SortedMap.toList x == SortedMap.toList y
  Func _ == Func _ = False
  _ == _ = False
 
export
truthiness : AST -> Bool
truthiness (Symbol "false") = False
truthiness (Symbol "nil") = False
truthiness _ = True

export
isList : AST -> Bool
isList (List False _) = True
isList _ = False

export
isVector : AST -> Bool
isVector (List True _) = True
isVector _ = False

export
isAtom : AST -> Bool
isAtom (Atom _) = True
isAtom _ = False

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
withGlobalEnv : MalM a -> MalM a
withGlobalEnv x = do
  env <- ask
  case last' env of
       Just ge => local (const [ge]) x
       Nothing => throwError $ Str "Internal error: no environment"

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
