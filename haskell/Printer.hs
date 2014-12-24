module Printer
( _pr_str, _pr_list )
where

import qualified Data.Map as Map

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
_pr_str _     (MalKeyword name) = ":" ++ name
_pr_str _     (MalNumber num) = show num
_pr_str _     (MalTrue) = "true"
_pr_str _     (MalFalse) = "false"
_pr_str _     (Nil) = "nil"
_pr_str pr    (MalList items) = "(" ++ (_pr_list pr " " items) ++ ")"
_pr_str pr    (MalVector items) = "[" ++ (_pr_list pr " " items) ++ "]"
_pr_str pr    (MalHashMap m) = "{" ++ (_pr_list pr " " (_flatTuples $ Map.assocs m)) ++ "}"
_pr_str _     (Func f) = "#<function>"
_pr_str _     (MalFunc {ast=ast, env=fn_env, params=params}) = "(fn* " ++ (show params) ++ " " ++ (show ast) ++ ")"

instance Show MalVal where show = _pr_str True

