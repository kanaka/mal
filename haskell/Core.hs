module Core
( ns )
where

import qualified Data.Map as Map

import Reader (read_str)
import Types
import Printer (_pr_str, _pr_list)

-- General functions

equal_Q args = case args of
    [a, b] -> return $ if a == b then MalTrue else MalFalse
    _ -> error $ "illegal arguments to ="

run_1 :: (MalVal -> MalVal) -> [MalVal] -> IO MalVal
run_1 f args = do
    case args of
         (x:[]) -> return $ f x
         _ -> error $ "function takes a single argument"


-- String functions

pr_str args = do
    return $ MalString $ _pr_list True " " args

str args = do
    return $ MalString $ _pr_list False "" args

prn args = do
    putStrLn $ _pr_list True " " args
    return Nil

println args = do
    putStrLn $ _pr_list False " " args
    return Nil

slurp args = do
    case args of
        ([MalString path]) -> do
            str <- readFile path
            return $ MalString str
        _ -> error $ "invalid arguments to slurp"

-- Numeric functions

num_op op args = case args of
    [MalNumber a, MalNumber b] -> return $ MalNumber $ op a b
    _ -> error $ "illegal arguments to number operation"

cmp_op op args = case args of
    [MalNumber a, MalNumber b] ->
        return $ if op a b then MalTrue else MalFalse
    _ -> error $ "illegal arguments to comparison operation"


-- List functions

list args = do
    return $ MalList args

-- Vector functions

vector args = do
    return $ MalVector args

-- Hash Map functions

hash_map args = do
    return $ MalHashMap $ Map.fromList $ _pairs args

-- Sequence functions

empty_Q Nil            = MalTrue
empty_Q (MalList [])   = MalTrue
empty_Q (MalVector []) = MalTrue
empty_Q _              = MalFalse

count Nil             = MalNumber 0
count (MalList lst)   = MalNumber $ length lst
count (MalVector lst) = MalNumber $ length lst
count _ = error $ "non-sequence passed to count"


ns = [
    ("=",  _func equal_Q),

    ("pr-str", _func pr_str),
    ("str", _func str),
    ("prn", _func prn),
    ("println", _func println),
    ("read-string", _func (\[(MalString s)] -> read_str s)),
    ("slurp", _func slurp),
    ("<",  _func $ cmp_op (<)),
    ("<=", _func $ cmp_op (<=)),
    (">",  _func $ cmp_op (>)),
    (">=", _func $ cmp_op (>=)),
    ("+",  _func $ num_op (+)),
    ("-",  _func $ num_op (-)),
    ("*",  _func $ num_op (*)),
    ("/",  _func $ num_op (div)),
    
    ("list",     _func $ list),
    ("list?",    _func $ run_1 _list_Q),
    ("vector",   _func $ vector),
    ("vector?",  _func $ run_1 $ _vector_Q),
    ("hash-map", _func $ hash_map),
    ("map?",     _func $ run_1 $ _hash_map_Q),
    
    ("empty?", _func $ run_1 $ empty_Q) ,
    ("count", _func $ run_1 $ count)]
