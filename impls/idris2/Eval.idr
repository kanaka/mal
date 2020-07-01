module Eval

import Types
import MonadTrans
import Data.List

-- Evaluate in the current eval mode.
export
eval : AST -> MalM AST
eval x = do
  e <- reader evalFunc
  e x

-- Run everything.
export
fullEval : AST -> MalM AST
fullEval (Symbol n) = lookup n
fullEval (Str s) = pure $ Str s
fullEval (Number x) = pure $ Number x
fullEval (Atom e) = pure $ Atom e
fullEval (WithMeta a b) = pure $ WithMeta a b -- TODO
fullEval (List False []) = pure $ List False []
fullEval (List False (f::xs)) = do
  Func _ f' <- fullEval f
    | _ => throwError $ Str "tried to call something that's not a function"
  f' xs
fullEval (List True xs) = map (List True) $ traverse fullEval xs
fullEval (Map m) = map Map $ traverse fullEval m
fullEval (Func m f) = pure $ Func m f

-- Run nothing.
-- Macros are run in don'tEval mode so their arguments don't get evaluated.
export
don'tEval : AST -> MalM AST
don'tEval = pure

-- Only run macros.
export
macroexpand : AST -> MalM AST
macroexpand (List False (Symbol n::xs)) = do
  Func isMacro f <- lookup n
    | _ => throwError $ Str "tried to call something that's not a function"
  if isMacro then f xs else pure $ List False (Symbol n::xs)
macroexpand x = pure x
