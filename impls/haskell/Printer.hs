module Printer
( _pr_str, _pr_list )
where

import qualified Data.Map as Map
import Data.IORef (readIORef)

import Types

_pr_list :: Bool -> String -> [MalVal] -> IO String
_pr_list _  _   []     = return $ []
_pr_list pr _   [x]    = _pr_str pr x
_pr_list pr sep (x:xs) = format <$> _pr_str pr x <*> _pr_list pr sep xs where
    format l r = l ++ sep ++ r

_flatTuples :: [(String, MalVal)] -> [MalVal]
_flatTuples ((a,b):xs) = MalString a : b : _flatTuples xs
_flatTuples _          = []

unescape :: Char -> String
unescape '\n' = "\\n"
unescape '\\' = "\\\\"
unescape '"'  = "\\\""
unescape c    = [c]

_pr_str :: Bool -> MalVal -> IO String
_pr_str _     (MalString (c : cs)) | c == keywordMagic
                                 = return $ ':' : cs
_pr_str True  (MalString str)    = return $ "\"" ++ concatMap unescape str ++ "\""
_pr_str False (MalString str)    = return str
_pr_str _     (MalSymbol name)   = return name
_pr_str _     (MalNumber num)    = return $ show num
_pr_str _     (MalBoolean True)  = return "true"
_pr_str _     (MalBoolean False) = return $ "false"
_pr_str _     Nil                = return "nil"
_pr_str pr (MalSeq _ (Vect False) items) = format <$> _pr_list pr " " items where
    format x = "(" ++ x ++ ")"
_pr_str pr (MalSeq _ (Vect True)  items) = format <$> _pr_list pr " " items where
    format x = "[" ++ x ++ "]"
_pr_str pr (MalHashMap _ m) = format <$> _pr_list pr " " (_flatTuples $ Map.assocs m) where
    format x = "{" ++ x ++ "}"
_pr_str pr (MalAtom _ r) = format  <$> (_pr_str pr =<< readIORef r) where
    format x = "(atom " ++ x ++ ")"
_pr_str _ (MalFunction {f_ast=Nil}) = pure "#<function>"
_pr_str _ (MalFunction {f_ast=a, f_params=p, macro=False}) = format <$> _pr_str True a where
    format x = "(fn* " ++ show p ++ " -> " ++ x ++ ")"
_pr_str _ (MalFunction {f_ast=a, f_params=p, macro=True})  = format <$> _pr_str True a where
    format x = "(macro* " ++ show p ++ " -> " ++ x ++ ")"
