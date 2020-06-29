module AST

import Data.SortedMap
import Data.Strings

public export
data AST = Symbol String -- Identifiers
         | Str String -- Includes keywords
         | Quasiquote AST
         | Quote AST
         | Unquote AST
         | SpliceUnquote AST
         | Deref AST
         | WithMeta AST AST
         | Number Integer
         | List (List AST)
         | Vector (List AST)
         | Map (SortedMap String AST)

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
