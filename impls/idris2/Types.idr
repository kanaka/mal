module Types

import Control.Monad.State
import public Data.SortedMap
import Data.Strings
import MonadTrans

mutual
  public export
  data AST = Symbol String -- Identifiers
           | Str String -- Includes keywords
           | Number Integer
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
  Env = SortedMap String AST

  public export
  MalM : Type -> Type
  MalM = StateT Env (ExceptT AST IO)

export
implementation Show AST where
  show (Symbol s) = s
  show (Str s) =
    case strUncons s of
         Just ('\xff', rest) => strCons ':' rest
         _ => show s
  show (Quasiquote x) = "(quasiquote " ++ show x ++ ")"
  show (Quote x) = "(quote " ++ show x ++ ")"
  show (Unquote x) = "(unquote " ++ show x ++ ")"
  show (SpliceUnquote x) = "(splice-unquote " ++ show x ++ ")"
  show (Deref x) = "(deref " ++ show x ++ ")"
  show (WithMeta a b) = "(with-meta " ++ show a ++ " " ++ show b ++ ")"
  show (Number x) = show x
  show (List xs) = "(" ++ unwords (map show xs) ++ ")"
  show (Vector xs) = "[" ++ unwords (map show xs) ++ "]"
  show (Map m) = "{" ++ unwords (concatMap (\(a, b) => [show (Str a), show b]) $ toList m) ++ "}"
  show (Func f) = "<function>"

public export
interface MalType a where
  toMalAst : a -> AST
  fromMalAst : AST -> Maybe a

public export
implementation MalType AST where
  toMalAst = id
  fromMalAst = Just

public export
implementation MalType Integer where
  toMalAst = Number
  fromMalAst (Number x) = Just x
  fromMalAst _ = Nothing

public export
implementation MalType String where
  toMalAst = Str
  fromMalAst (Str s) = Just s
  fromMalAst _ = Nothing

public export
implementation MalType a => MalType (List a) where
  toMalAst = Vector . map toMalAst -- Should this be List or Vector?
  fromMalAst (List xs) = traverse fromMalAst xs
  fromMalAst _ = Nothing

public export
implementation MalType a => MalType (SortedMap String a) where
  toMalAst = Map . map toMalAst
  fromMalAst (Map m) = traverse fromMalAst m
  fromMalAst _ = Nothing

public export
interface MalFunction a where
  toMalFunc : a -> List AST -> MalM AST

public export
implementation MalType a => MalFunction a where
  toMalFunc x [] = pure $ toMalAst x
  toMalFunc _ _ = throwError $ Str "Too many arguments"

public export
implementation MalType a => MalFunction (MalM a) where
  toMalFunc x [] = map toMalAst x
  toMalFunc _ _ = throwError $ Str "Too many arguments"

public export
implementation (MalType a, MalFunction b) => MalFunction (a -> b) where
  toMalFunc _ [] = throwError $ Str "Too few arguments"
  toMalFunc f (x::xs) = case fromMalAst x of
                             Just x' => toMalFunc (f x') xs
                             Nothing => throwError $ Str "Wrong argument type"
