module Printer
( _pr_str, _pr_list )
where

import qualified Data.Map.Strict as Map
import Data.IORef (readIORef)
import Data.List (intercalate)

import Types

_pr_list :: Bool -> String -> [MalVal] -> IO String
_pr_list pr sep = fmap (intercalate sep) . mapM (_pr_str pr)

enclose :: String -> String -> String -> String
enclose open close middle = open ++ middle ++ close

escape :: Char -> String -> String
escape '\n' acc = '\\' : 'n'  : acc
escape '\\' acc = '\\' : '\\' : acc
escape '"'  acc = '\\' : '"'  : acc
escape c    acc = c           : acc

_pr_str :: Bool -> MalVal -> IO String
_pr_str _     (MalKeyword kwd)   = return $ ':' : kwd
_pr_str True  (MalString str)    = return $ enclose "\"" "\"" $ foldr escape [] str
_pr_str False (MalString str)    = return str
_pr_str _     (MalSymbol name)   = return name
_pr_str _     (MalNumber num)    = return $ show num
_pr_str _     (MalBoolean True)  = return "true"
_pr_str _     (MalBoolean False) = return "false"
_pr_str _     Nil                = return "nil"
_pr_str pr (MalSeq _ (Vect False) xs) = enclose "(" ")" <$> _pr_list pr " " xs
_pr_str pr (MalSeq _ (Vect True)  xs) = enclose "[" "]" <$> _pr_list pr " " xs
_pr_str pr (MalHashMap _ m)      = enclose "{" "}" <$> _pr_list pr " "
                                    (Map.foldMapWithKey (\k v -> [decodeKey k, v]) m)
_pr_str pr (MalAtom _ r)         = enclose "(atom " ")" <$> (_pr_str pr =<< readIORef r)
_pr_str _ (MalFunction _ _)      = return "<fn>"
_pr_str _ (MalMacro _)           = return "<macro>"
