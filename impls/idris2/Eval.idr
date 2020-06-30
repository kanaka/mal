module Eval

import Types
import MonadTrans

public export
eval : AST -> MalM AST
eval (Symbol n) = do
  Just x <- gets (lookup n)
    | Nothing => throwError $ Str $ "unrecognized ident " ++ n
  pure x
eval (Str s) = pure $ Str s
eval (Number x) = pure $ Number x
eval (Quasiquote x) = pure x -- TODO
eval (Quote x) = pure x -- TODO
eval (Unquote x) = pure x -- TODO
eval (SpliceUnquote x) = pure x -- TODO
eval (Deref x) = pure x -- TODO
eval (WithMeta a b) = pure $ WithMeta a b -- TODO
eval (List []) = pure $ List []
eval (List xs) = do
  Func f'::xs' <- traverse eval xs
    | _ => throwError $ Str "tried to call something that's not a function"
  f' xs'
eval (Vector xs) = map Vector $ traverse eval xs
eval (Map m) = map Map $ traverse eval m
eval (Func f) = pure $ Func f

-- Builtins

public export
startingEnv : Env
startingEnv = fromList [
  ("+", Func $ toMalFunc ((+) {ty=Integer})),
  ("-", Func $ toMalFunc ((-) {ty=Integer})),
  ("*", Func $ toMalFunc ((*) {ty=Integer})),
  ("/", Func $ toMalFunc (div {ty=Integer}))
]
