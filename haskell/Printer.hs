module Printer
( _pr_str )
where

import qualified Data.Map as Map

import Types

_pr_list :: [MalVal] -> String
_pr_list = unwords . map _pr_str

_flatTuples ((a,b):xs) = MalString a : b : _flatTuples xs
_flatTuples _          = []

unescape chr = case chr of
    '\n' -> "\\n"
    '\\' -> "\\\\"
    '"'  -> "\\\""
    c    -> [c]

_pr_str :: MalVal -> String
_pr_str (MalString ('\x029e':str)) = ":" ++ str
_pr_str (MalString str) = "\"" ++ concatMap unescape str ++ "\""
_pr_str (MalSymbol name) = name
_pr_str (MalKeyword name) = ":" ++ name
_pr_str (MalNumber num) = show num
_pr_str (MalTrue) = "true"
_pr_str (MalFalse) = "false"
_pr_str (Nil) = "nil"
_pr_str (MalList items) = "(" ++ _pr_list items ++ ")"
_pr_str (MalVector items) = "[" ++ _pr_list items ++ "]"
_pr_str (MalHashMap m) = "{" ++ _pr_list (_flatTuples $ Map.assocs m) ++ "}"
_pr_str (MalFunc f) = "#<function>"

instance Show MalVal where show = _pr_str

