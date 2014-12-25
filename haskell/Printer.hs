module Printer
( _pr_str, _pr_list )
where

import qualified Data.Map as Map
import Data.IORef (readIORef)
import System.IO.Unsafe (unsafePerformIO)

import Types

--concat (map (++ delim) list)
--join [] delim = []
--join (x:xs) delim = x ++ delim  ++ join xs delim


_pr_list :: Bool -> String -> [MalVal] -> String
_pr_list pr sep [] = []
_pr_list pr sep (x:[]) = (_pr_str pr x)
_pr_list pr sep (x:xs) = (_pr_str pr x) ++ sep ++ (_pr_list pr sep xs)

_flatTuples ((a,b):xs) = MalString a : b : _flatTuples xs
_flatTuples _          = []

unescape chr = case chr of
    '\n' -> "\\n"
    '\\' -> "\\\\"
    '"'  -> "\\\""
    c    -> [c]

_pr_str :: Bool -> MalVal -> String
_pr_str _     (MalString ('\x029e':str)) = ":" ++ str
_pr_str True  (MalString str) = "\"" ++ concatMap unescape str ++ "\""
_pr_str False (MalString str) = str
_pr_str _     (MalSymbol name) = name
_pr_str _     (MalNumber num) = show num
_pr_str _     (MalTrue) = "true"
_pr_str _     (MalFalse) = "false"
_pr_str _     (Nil) = "nil"
_pr_str pr    (MalList items _) = "(" ++ (_pr_list pr " " items) ++ ")"
_pr_str pr    (MalVector items _) = "[" ++ (_pr_list pr " " items) ++ "]"
_pr_str pr    (MalHashMap m _) = "{" ++ (_pr_list pr " " (_flatTuples $ Map.assocs m)) ++ "}"
_pr_str pr    (MalAtom r _) = "(atom " ++ (_pr_str pr (unsafePerformIO (readIORef r))) ++ ")"
_pr_str _     (Func f _) = "#<function>"
_pr_str _     (MalFunc {ast=ast, env=fn_env, params=params}) = "(fn* " ++ (show params) ++ " " ++ (show ast) ++ ")"

instance Show MalVal where show = _pr_str True

