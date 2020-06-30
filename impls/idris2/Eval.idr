module Eval

import Types
import MonadTrans

public export
eval : AST -> MalM AST
eval (Symbol n) = lookup n
eval (Str s) = pure $ Str s
eval (Number x) = pure $ Number x
eval (Boolean b) = pure $ Boolean b
eval Nil = pure Nil
eval (Quasiquote x) = pure x -- TODO
eval (Quote x) = pure x -- TODO
eval (Unquote x) = pure x -- TODO
eval (SpliceUnquote x) = pure x -- TODO
eval (Deref x) = pure x -- TODO
eval (WithMeta a b) = pure $ WithMeta a b -- TODO
eval (List []) = pure $ List []
eval (List (f::xs)) = do
  Func f' <- eval f
    | _ => throwError $ Str "tried to call something that's not a function"
  f' xs
eval (Vector xs) = map Vector $ traverse eval xs
eval (Map m) = map Map $ traverse eval m
eval (Func f) = pure $ Func f

-- Builtins
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
  toMalFunc _ xs = do
    traverse eval xs -- (+ 1 2 (println "hi")) should print hi
    throwError $ Str "Too many arguments"

public export
implementation MalType a => MalFunction (MalM a) where
  toMalFunc x [] = map toMalAst x
  toMalFunc _ xs = do
    traverse eval xs
    throwError $ Str "Too many arguments"

public export
implementation (MalType a, MalFunction b) => MalFunction (a -> b) where
  toMalFunc _ [] = throwError $ Str "Too few arguments"
  toMalFunc f (x::xs) = do
    Just x' <- map fromMalAst $ eval x
      | Nothing => traverse_ eval xs *> throwError (Str "Wrong argument type")
    toMalFunc (f x') xs

defBuiltin : List AST -> MalM AST
defBuiltin [] = throwError $ Str "def!: too few arguments"
defBuiltin [_] = throwError $ Str "def!: too few arguments"
defBuiltin [Symbol n, x] = do
  x' <- eval x
  insert n x'
  pure x'
defBuiltin [_, _] = throwError $ Str "def!: expecting symbol"
defBuiltin _ = throwError $ Str "def!: too many arguments"

letBuiltin : List AST -> MalM AST
letBuiltin [] = throwError $ Str "let*: too few arguments"
letBuiltin [_] = throwError $ Str "let*: too few arguments"
letBuiltin [defs, ans] = do
  defs' <- the (MalM _) $ case defs of
    List xs => pure xs
    Vector xs => pure xs
    _ => throwError $ Str "let*: expecting vector or list"
  withLocalEnv $ go defs'
  where
    go : List AST -> MalM AST
    go [] = eval ans
    go (Symbol n::x::rest) = do
      x' <- eval x
      insert n x'
      go rest
    go _ = throwError $ Str "let*: malformed arguments"
letBuiltin _ = throwError $ Str "let*: too many arguments"

public export
getStartingEnv : IO Env
getStartingEnv = map pure $ newIORef startingEnv
  where
    startingEnv : SortedMap String AST
    startingEnv = fromList [
      ("nil", Nil),
      ("true", Boolean True),
      ("false", Boolean True),
      ("+", Func $ toMalFunc ((+) {ty=Integer})),
      ("-", Func $ toMalFunc ((-) {ty=Integer})),
      ("*", Func $ toMalFunc ((*) {ty=Integer})),
      ("/", Func $ toMalFunc (div {ty=Integer})),
      ("def!", Func defBuiltin),
      ("let*", Func letBuiltin)
    ]
