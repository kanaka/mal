module Core
( ns )
where

import System.IO (hFlush, stdout)
import Control.Monad.Trans (liftIO)
import qualified Data.Map as Map
import Data.Foldable (foldlM)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.IORef (newIORef, readIORef, writeIORef)

import Readline (readline)
import Reader (read_str)
import Types
import Printer (_pr_list)

-- General functions

equal_Q :: [MalVal] -> IOThrows MalVal
equal_Q [a, b] = return $ if a == b then MalTrue else MalFalse
equal_Q _ = throwStr "illegal arguments to ="

run_1 :: (MalVal -> MalVal) -> [MalVal] -> IOThrows MalVal
run_1 f (x:[]) = return $ f x
run_1 _ _ = throwStr "function takes a single argument"

-- Error/Exception functions

throw :: [MalVal] -> IOThrows MalVal
throw (mv:[]) = throwMalVal mv
throw _ = throwStr "illegal arguments to throw"

-- Scalar functions

symbol :: [MalVal] -> IOThrows MalVal
symbol [MalString s] = return $ MalSymbol s
symbol _ = throwStr "symbol called with non-string"

keyword :: [MalVal] -> IOThrows MalVal
keyword [k@(MalString ('\x029e' : _))] = return k
keyword [MalString s] = return $ MalString $ '\x029e' : s
keyword _ = throwStr "keyword called with non-string"


-- String functions

pr_str :: [MalVal] -> IOThrows MalVal
pr_str args = do
    return $ MalString $ _pr_list True " " args

str :: [MalVal] -> IOThrows MalVal
str args = do
    return $ MalString $ _pr_list False "" args

prn :: [MalVal] -> IOThrows MalVal
prn args = do
    liftIO $ putStrLn $ _pr_list True " " args
    liftIO $ hFlush stdout
    return Nil

println :: [MalVal] -> IOThrows MalVal
println args = do
    liftIO $ putStrLn $ _pr_list False " " args
    liftIO $ hFlush stdout
    return Nil

slurp :: [MalVal] -> IOThrows MalVal
slurp ([MalString path]) = do
    contents <- liftIO $ readFile path
    return $ MalString contents
slurp _ = throwStr "invalid arguments to slurp"

do_readline :: [MalVal] -> IOThrows MalVal
do_readline ([MalString prompt]) = do
    maybeLine <- liftIO $ readline prompt
    case maybeLine of
        Nothing -> throwStr "readline failed"
        Just line -> return $ MalString line
do_readline _ = throwStr "invalid arguments to readline"

read_string :: [MalVal] -> IOThrows MalVal
read_string [MalString s] = read_str s
read_string _ = throwStr "invalid read-string"

-- Numeric functions

num_op :: (Int -> Int -> Int) -> [MalVal] -> IOThrows MalVal
num_op op [MalNumber a, MalNumber b] = do
    return $ MalNumber $ op a b
num_op _ _ = throwStr "illegal arguments to number operation"

cmp_op :: (Int -> Int -> Bool) -> [MalVal] -> IOThrows MalVal
cmp_op op [MalNumber a, MalNumber b] = do
    return $ if op a b then MalTrue else MalFalse
cmp_op _ _ = throwStr "illegal arguments to comparison operation"

time_ms :: [MalVal] -> IOThrows MalVal
time_ms _ = do
   t <- liftIO $ getPOSIXTime
   return $ MalNumber $ round (t * 1000)


-- List functions

list :: [MalVal] -> IOThrows MalVal
list args = return $ MalList args Nil

-- Vector functions

vector :: [MalVal] -> IOThrows MalVal
vector args = return $ MalVector args Nil

-- Hash Map functions

_pairup :: [MalVal] -> IOThrows [(String, MalVal)]
_pairup [] = return []
_pairup (MalString x:y:xs) = do
    pairs <- _pairup xs
    return $ (x,y):pairs
_pairup _ = throwStr "invalid hash-map or assoc"

hash_map :: [MalVal] -> IOThrows MalVal
hash_map args = do
    pairs <- _pairup args
    return $ MalHashMap (Map.fromList pairs) Nil

assoc :: [MalVal] -> IOThrows MalVal
assoc (MalHashMap hm _:kvs) = do
    pairs <- _pairup kvs
    return $ MalHashMap (Map.union (Map.fromList pairs) hm) Nil
assoc _ = throwStr "invalid call to assoc"

dissoc :: [MalVal] -> IOThrows MalVal
dissoc (MalHashMap hm _:ks) = do
    let remover acc (MalString k) = return $ Map.delete k acc
        remover _ _ = throwStr "invalid dissoc"
    newMap <- foldlM remover hm ks
    return $ MalHashMap newMap Nil
dissoc _ = throwStr "invalid call to dissoc"

get :: [MalVal] -> IOThrows MalVal
get (MalHashMap hm _:MalString k:[]) = do
    case Map.lookup k hm of
        Just mv -> return mv
        Nothing -> return Nil
get [Nil, MalString _] = return Nil
get _ = throwStr "invalid call to get"

contains_Q :: [MalVal] -> IOThrows MalVal
contains_Q (MalHashMap hm _:MalString k:[]) = do
    if Map.member k hm then return MalTrue
    else return MalFalse
contains_Q [Nil, MalString _] = return MalFalse
contains_Q _ = throwStr "invalid call to contains?"

keys :: [MalVal] -> IOThrows MalVal
keys (MalHashMap hm _:[]) = do
    return $ MalList (map MalString (Map.keys hm)) Nil
keys _ = throwStr "invalid call to keys"

vals :: [MalVal] -> IOThrows MalVal
vals (MalHashMap hm _:[]) = do
    return $ MalList (Map.elems hm) Nil
vals _ = throwStr "invalid call to vals"


-- Sequence functions

_sequential_Q :: MalVal -> MalVal
_sequential_Q (MalList _ _) = MalTrue
_sequential_Q (MalVector _ _) = MalTrue
_sequential_Q _ = MalFalse

cons :: [MalVal] -> IOThrows MalVal
cons [x, Nil            ] = return (MalList [x]       Nil)
cons [x, MalList   lst _] = return (MalList (x : lst) Nil)
cons [x, MalVector lst _] = return (MalList (x : lst) Nil)
cons _ = throwStr "invalid cons"

concat1 :: [MalVal] -> MalVal -> IOThrows [MalVal]
concat1 a (MalList   lst _) = return $ a ++ lst
concat1 a (MalVector lst _) = return $ a ++ lst
concat1 _ _ = throwStr "invalid concat"

do_concat :: [MalVal] -> IOThrows MalVal
do_concat args = do
    xs <- foldlM concat1 [] args
    return $ MalList xs Nil

nth :: [MalVal] -> IOThrows MalVal
nth ((MalList lst _):(MalNumber idx):[]) = do
    if idx < length lst then return $ lst !! idx
    else throwStr "nth: index out of range"
nth ((MalVector lst _):(MalNumber idx):[]) = do
    if idx < length lst then return $ lst !! idx
    else throwStr "nth: index out of range"
nth _ = throwStr "invalid call to nth"

first :: [MalVal] -> IOThrows MalVal
first [Nil                ] = return Nil
first [MalList   [] _     ] = return Nil
first [MalVector [] _     ] = return Nil
first [MalList   (x : _) _] = return x
first [MalVector (x : _) _] = return x
first _ = throwStr "invalid first"

rest :: [MalVal] -> IOThrows MalVal
rest [Nil                 ] = return $ MalList [] Nil
rest [MalList   (_ : xs) _] = return $ MalList xs Nil
rest [MalVector (_ : xs) _] = return $ MalList xs Nil
rest _ = throwStr "invalid rest"

empty_Q :: MalVal -> MalVal
empty_Q Nil              = MalTrue
empty_Q (MalList [] _)   = MalTrue
empty_Q (MalVector [] _) = MalTrue
empty_Q _                = MalFalse

count :: [MalVal] -> IOThrows MalVal
count (Nil:[])             = return $ MalNumber 0
count (MalList lst _:[])   = return $ MalNumber $ length lst
count (MalVector lst _:[]) = return $ MalNumber $ length lst
count _ = throwStr $ "non-sequence passed to count"

apply :: [MalVal] -> IOThrows MalVal
apply args = do
    f <- _get_call args
    lst <- _to_list (last args)
    f $ (init (drop 1 args)) ++ lst

do_map :: [MalVal] -> IOThrows MalVal
do_map args = do
    f <- _get_call args
    lst <- _to_list (args !! 1)
    do new_lst <- mapM (\x -> f [x]) lst
       return $ MalList new_lst Nil

conj :: [MalVal] -> IOThrows MalVal
conj ((MalList lst _):args) = return $ MalList ((reverse args) ++ lst) Nil
conj ((MalVector lst _):args) = return $ MalVector (lst ++ args) Nil
conj _ = throwStr $ "illegal arguments to conj"

do_seq :: [MalVal] -> IOThrows MalVal
do_seq [MalList [] _]         = return Nil
do_seq [l@(MalList _ _)]      = return l
do_seq (MalVector [] _:[])    = return $ Nil
do_seq (MalVector lst _:[])   = return $ MalList lst Nil
do_seq (MalString []:[])      = return $ Nil
do_seq (MalString s:[])       = return $ MalList [MalString [c] | c <- s] Nil
do_seq (Nil:[])               = return $ Nil
do_seq _                      = throwStr $ "seq: called on non-sequence"

-- Metadata functions

with_meta :: [MalVal] -> IOThrows MalVal
with_meta ((MalList lst _):m:[])   = return $ MalList lst m
with_meta ((MalVector lst _):m:[]) = return $ MalVector lst m
with_meta ((MalHashMap hm _):m:[]) = return $ MalHashMap hm m
with_meta ((MalAtom atm _):m:[])   = return $ MalAtom atm m
with_meta ((Func f _):m:[])        = return $ Func f m
with_meta [f@(MalFunc {}), m]      = return $ f {meta=m}
with_meta _ = throwStr $ "invalid with-meta call"

do_meta :: [MalVal] -> IOThrows MalVal
do_meta ((MalList _ m):[])      = return m
do_meta ((MalVector _ m):[])    = return m
do_meta ((MalHashMap _ m):[])   = return m
do_meta ((MalAtom _ m):[])      = return m
do_meta ((Func _ m):[])         = return m
do_meta ((MalFunc {meta=m}):[]) = return m
do_meta _ = throwStr $ "invalid meta call"

-- Atom functions

atom :: [MalVal] -> IOThrows MalVal
atom (val:[]) = do
    ref <- liftIO $ newIORef val
    return $ MalAtom ref Nil
atom _ = throwStr "invalid atom call"

deref :: [MalVal] -> IOThrows MalVal
deref (MalAtom ref _:[]) = do
    val <- liftIO $ readIORef ref
    return val
deref _ = throwStr "invalid deref call"

reset_BANG :: [MalVal] -> IOThrows MalVal
reset_BANG (MalAtom ref _:val:[]) = do
    liftIO $ writeIORef ref $ val
    return val
reset_BANG _ = throwStr "invalid reset!"

swap_BANG :: [MalVal] -> IOThrows MalVal
swap_BANG (MalAtom ref _:args) = do
    val <- liftIO $ readIORef ref
    f <- _get_call args
    new_val <- f $ [val] ++ (tail args)
    _ <- liftIO $ writeIORef ref $ new_val
    return new_val
swap_BANG _ = throwStr "invalid swap!"

ns :: [(String, MalVal)]
ns = [
    ("=",  _func equal_Q),
    ("throw", _func throw),
    ("nil?", _func $ run_1 $ _nil_Q),
    ("true?", _func $ run_1 $ _true_Q),
    ("false?", _func $ run_1 $ _false_Q),
    ("string?", _func $ run_1 $ _string_Q),
    ("symbol", _func $ symbol),
    ("symbol?", _func $ run_1 $ _symbol_Q),
    ("keyword", _func $ keyword),
    ("keyword?", _func $ run_1 $ _keyword_Q),
    ("number?", _func $ run_1 $ _number_Q),
    ("fn?", _func $ run_1 $ _fn_Q),
    ("macro?", _func $ run_1 $ _macro_Q),

    ("pr-str", _func pr_str),
    ("str", _func str),
    ("prn", _func prn),
    ("println", _func println),
    ("readline", _func do_readline),
    ("read-string", _func read_string),
    ("slurp", _func slurp),

    ("<",  _func $ cmp_op (<)),
    ("<=", _func $ cmp_op (<=)),
    (">",  _func $ cmp_op (>)),
    (">=", _func $ cmp_op (>=)),
    ("+",  _func $ num_op (+)),
    ("-",  _func $ num_op (-)),
    ("*",  _func $ num_op (*)),
    ("/",  _func $ num_op (div)),
    ("time-ms", _func $ time_ms),

    ("list",     _func $ list),
    ("list?",    _func $ run_1 _list_Q),
    ("vector",   _func $ vector),
    ("vector?",  _func $ run_1 _vector_Q),
    ("hash-map", _func $ hash_map),
    ("map?",     _func $ run_1 _hash_map_Q),
    ("assoc",    _func $ assoc),
    ("dissoc",   _func $ dissoc),
    ("get",      _func $ get),
    ("contains?",_func $ contains_Q),
    ("keys",     _func $ keys),
    ("vals",     _func $ vals),

    ("sequential?", _func $ run_1 _sequential_Q),
    ("cons", _func $ cons),
    ("concat", _func $ do_concat),
    ("nth", _func nth),
    ("first", _func $ first),
    ("rest", _func $ rest),
    ("empty?", _func $ run_1 $ empty_Q),
    ("count", _func $ count),
    ("apply", _func $ apply),
    ("map", _func $ do_map),

    ("conj", _func $ conj),
    ("seq", _func $ do_seq),

    ("with-meta", _func $ with_meta),
    ("meta",      _func $ do_meta),
    ("atom",      _func $ atom),
    ("atom?",     _func $ run_1 _atom_Q),
    ("deref",     _func $ deref),
    ("reset!",    _func $ reset_BANG),
    ("swap!",     _func $ swap_BANG)]
