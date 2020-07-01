module Eval

import Types
import MonadTrans
import Data.List

public export
eval : AST -> MalM AST
eval (Symbol n) = lookup n
eval (Str s) = pure $ Str s
eval (Number x) = pure $ Number x
eval (Atom e) = pure $ Atom e
eval (WithMeta a b) = pure $ WithMeta a b -- TODO
eval (List False []) = pure $ List False []
eval (List False (f::xs)) = do
  Func f' <- eval f
    | _ => throwError $ Str "tried to call something that's not a function"
  f' xs
eval (List True xs) = map (List True) $ traverse eval xs
eval (Map m) = map Map $ traverse eval m
eval (Func f) = pure $ Func f
