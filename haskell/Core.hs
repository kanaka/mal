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

run_2 :: (MalVal -> MalVal -> MalVal) -> [MalVal] -> IO MalVal
run_2 f args = do
    case args of
         (x:y:[]) -> return $ f x y
         _ -> error $ "function takes a two arguments"


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

cons x Nil = MalList [x]
cons x (MalList lst) = MalList $ x:lst
cons x (MalVector lst) = MalList $ x:lst

concat1 a (MalList lst) = a ++ lst
concat1 a (MalVector lst) = a ++ lst
do_concat args = return $ MalList $ foldl concat1 [] args

nth args = do
    case args of
        (MalList lst):(MalNumber idx):[] ->
            if idx < length lst then return $ lst !! idx
            else error "nth: index out of range"
        (MalVector lst):(MalNumber idx):[] ->
            if idx < length lst then return $ lst !! idx
            else error "nth: index out of range"

first (MalList lst) = if length lst > 0 then lst !! 0 else Nil
first (MalVector lst) = if length lst > 0 then lst !! 0 else Nil

rest (MalList lst) = MalList $ drop 1 lst
rest (MalVector lst) = MalList $ drop 1 lst

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
    
    ("cons", _func $ run_2 $ cons),
    ("concat", _func $ do_concat),
    ("nth", _func nth),
    ("first", _func $ run_1 $ first),
    ("rest", _func $ run_1 $ rest),
    ("empty?", _func $ run_1 $ empty_Q) ,
    ("count", _func $ run_1 $ count)]
