||| Typeclasses that describe types that correspond to Mal data
module Class

import Eval
import Types

export
interface MalType a where
  toMalAst : a -> AST
  fromMalAst : AST -> Maybe a

export
implementation MalType AST where
  toMalAst = id
  fromMalAst = Just

export
implementation MalType () where
  toMalAst () = Symbol "nil"
  fromMalAst (Symbol "nil") = Just ()
  fromMalAst _ = Nothing

export
implementation MalType Integer where
  toMalAst = Number
  fromMalAst (Number x) = Just x
  fromMalAst _ = Nothing

export
implementation MalType String where
  toMalAst = Str
  fromMalAst (Str s) = Just s
  fromMalAst _ = Nothing

export
implementation MalType Bool where
  toMalAst True = Symbol "true"
  toMalAst False = Symbol "false"
  fromMalAst (Symbol "true") = Just True
  fromMalAst (Symbol "false") = Just False
  -- fromMalAst (Symbol "nil") = Just False -- Maybe?
  fromMalAst _ = Nothing

export
implementation MalType (IORef AST) where
  toMalAst = Atom
  fromMalAst (Atom a) = Just a
  fromMalAst _ = Nothing

export
implementation MalType a => MalType (List a) where
  toMalAst = List False . map toMalAst
  fromMalAst (List _ xs) = traverse fromMalAst xs
  fromMalAst (Symbol "nil") = Just []
  fromMalAst _ = Nothing

export
implementation MalType (SortedMap String AST) where
  toMalAst = Map
  fromMalAst (Map m) = Just m
  fromMalAst (Symbol "nil") = Just empty
  fromMalAst _ = Nothing

export
interface MalFunction a where
  toMalFunc : a -> List AST -> MalM AST

export
implementation MalType a => MalFunction a where
  toMalFunc x [] = pure $ toMalAst x
  toMalFunc _ xs = do
    traverse eval xs -- (+ 1 2 (println "hi")) should print hi
    throwError $ Str "Too many arguments"

export
implementation MalType a => MalFunction (MalM a) where
  toMalFunc x [] = map toMalAst x
  toMalFunc _ xs = do
    traverse eval xs
    throwError $ Str "Too many arguments"

export
implementation (MalType a, MalFunction b) => MalFunction (a -> b) where
  toMalFunc _ [] = throwError $ Str "Too few arguments"
  toMalFunc f (x::xs) = do
    Just x' <- map fromMalAst $ eval x
      | Nothing => traverse_ eval xs *> throwError (Str "Wrong argument type")
    toMalFunc (f x') xs
