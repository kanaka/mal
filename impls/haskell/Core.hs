module Core
( ns )
where

import System.IO (hFlush, stdout)
import Control.Monad.Except (throwError)
import Control.Monad.Trans (liftIO)
import qualified Data.Map.Strict as Map
import Data.Foldable (foldlM)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.IORef (newIORef, readIORef, writeIORef)

import Readline (readline)
import Reader (read_str)
import Types
import Printer (_pr_list)

-- General functions

equal_Q :: Fn
equal_Q [a, b] = return $ MalBoolean $ a == b
equal_Q _ = throwStr "illegal arguments to ="

-- Error/Exception functions

throw :: Fn
throw [mv] = throwError mv
throw _ = throwStr "illegal arguments to throw"

-- Unary predicates

pred1 :: String -> (MalVal -> Bool) -> (String, Fn)
pred1 name op = (name, fn) where
  fn :: Fn
  fn [a] = return $ MalBoolean $ op a
  fn _ = throwStr $ "illegal arguments to " ++ name

atom_Q :: MalVal -> Bool
atom_Q (MalAtom _ _) = True
atom_Q _             = False

false_Q :: MalVal -> Bool
false_Q (MalBoolean False) = True
false_Q _                  = False

fn_Q :: MalVal -> Bool
fn_Q (MalFunction _ _) = True
fn_Q _                 = False

macro_Q :: MalVal -> Bool
macro_Q (MalMacro _) = True
macro_Q _            = False

map_Q :: MalVal -> Bool
map_Q (MalHashMap _ _) = True
map_Q _                = False

keyword_Q :: MalVal -> Bool
keyword_Q (MalKeyword _) = True
keyword_Q _              = False

list_Q :: MalVal -> Bool
list_Q (MalSeq _ (Vect False) _) = True
list_Q _                         = False

nil_Q :: MalVal -> Bool
nil_Q Nil = True
nil_Q _   = False

number_Q :: MalVal -> Bool
number_Q (MalNumber _) = True
number_Q _             = False

string_Q :: MalVal -> Bool
string_Q (MalString _) = True
string_Q _             = False

symbol_Q :: MalVal -> Bool
symbol_Q (MalSymbol _) = True
symbol_Q _             = False

true_Q :: MalVal -> Bool
true_Q (MalBoolean True) = True
true_Q _                 = False

vector_Q :: MalVal -> Bool
vector_Q (MalSeq _ (Vect True) _) = True
vector_Q _                        = False

-- Scalar functions

symbol :: Fn
symbol [MalString s] = return $ MalSymbol s
symbol _ = throwStr "symbol called with non-string"

keyword :: Fn
keyword [kw@(MalKeyword _)] = return kw
keyword [MalString s] = return $ MalKeyword s
keyword _ = throwStr "keyword called with non-string"

-- String functions

pr_str :: Fn
pr_str args = liftIO $ MalString <$> _pr_list True " " args

str :: Fn
str args = liftIO $ MalString <$> _pr_list False "" args

prn :: Fn
prn args = liftIO $ do
    putStrLn =<< _pr_list True " " args
    hFlush stdout
    return Nil

println :: Fn
println args = liftIO $ do
    putStrLn =<< _pr_list False " " args
    hFlush stdout
    return Nil

slurp :: Fn
slurp [MalString path] = MalString <$> liftIO (readFile path)
slurp _ = throwStr "invalid arguments to slurp"

do_readline :: Fn
do_readline [MalString prompt] = do
    maybeLine <- liftIO $ readline prompt
    case maybeLine of
        Nothing -> throwStr "readline failed"
        Just line -> return $ MalString line
do_readline _ = throwStr "invalid arguments to readline"

read_string :: Fn
read_string [MalString s] = read_str s
read_string _ = throwStr "invalid read-string"

-- Numeric functions

num_op :: String -> (Int -> Int -> Int) -> (String, Fn)
num_op name op = (name, fn) where
  fn :: Fn
  fn [MalNumber a, MalNumber b] = return $ MalNumber $ op a b
  fn _ = throwStr $ "illegal arguments to " ++ name

cmp_op :: String -> (Int -> Int -> Bool) -> (String, Fn)
cmp_op name op = (name, fn) where
  fn :: Fn
  fn [MalNumber a, MalNumber b] = return $ MalBoolean $ op a b
  fn _ = throwStr $ "illegal arguments to " ++ name

time_ms :: Fn
time_ms [] = MalNumber . round . (* 1000) <$> liftIO getPOSIXTime
time_ms _ = throwStr "invalid time-ms"


-- List functions

list :: Fn
list = return . toList

-- Vector functions

vector :: Fn
vector = return . MalSeq (MetaData Nil) (Vect True)

-- Hash Map functions

hash_map :: Fn
hash_map kvs = case kv2map Map.empty kvs of
        Just m     -> return m
        Nothing    -> throwStr "invalid call to hash-map"

assoc :: Fn
assoc (MalHashMap _ hm : kvs) = case kv2map hm kvs of
        Just m     -> return m
        Nothing    -> throwStr "invalid assoc"
assoc _ = throwStr "invalid call to assoc"

remover :: Map.Map String MalVal -> MalVal -> IOThrows (Map.Map String MalVal)
remover acc key = case encodeKey key of
    Nothing      -> throwStr "invalid dissoc"
    Just encoded -> return $ Map.delete encoded acc

dissoc :: Fn
dissoc (MalHashMap _ hm : ks) = MalHashMap (MetaData Nil) <$> foldlM remover hm ks
dissoc _ = throwStr "invalid call to dissoc"

get :: Fn
get [MalHashMap _ hm, k] = case encodeKey k of
    Nothing  -> throwStr "invalid call to get"
    Just key -> case Map.lookup key hm of
        Just mv -> return mv
        Nothing -> return Nil
get [Nil, MalString _] = return Nil
get _ = throwStr "invalid call to get"

contains_Q :: Fn
contains_Q [MalHashMap _ hm, k] = case encodeKey k of
  Just key -> return $ MalBoolean $ Map.member key  hm
  Nothing  -> throwStr "invalid call to contains?"
contains_Q [Nil, MalString _] = return $ MalBoolean False
contains_Q [Nil, MalSymbol _] = return $ MalBoolean False
contains_Q _ = throwStr "invalid call to contains?"

keys :: Fn
keys [MalHashMap _ hm] = return $ toList $ decodeKey <$> Map.keys hm
keys _ = throwStr "invalid call to keys"

vals :: Fn
vals [MalHashMap _ hm] = return $ toList $ Map.elems hm
vals _ = throwStr "invalid call to vals"

-- Sequence functions

sequential_Q :: MalVal -> Bool
sequential_Q (MalSeq _ _ _) = True
sequential_Q _              = False

cons :: Fn
cons [x, Nil           ] = return $ toList [x]
cons [x, MalSeq _ _ lst] = return $ toList (x : lst)
cons _ = throwStr "illegal call to cons"

unwrapSeq :: MalVal -> IOThrows [MalVal]
unwrapSeq (MalSeq _ _ xs) = return xs
unwrapSeq _ = throwStr "invalid concat"

do_concat :: Fn
do_concat args = toList . concat <$> mapM unwrapSeq args

vec :: Fn
vec [MalSeq _ _ xs] = return $ MalSeq (MetaData Nil) (Vect True) xs
vec [_]             = throwStr "vec: arg type"
vec _               = throwStr "vec: arg count"

nth :: Fn
nth [MalSeq _ _ lst, MalNumber idx] =
    case drop idx lst of
        x : _ -> return x
        []    -> throwStr "nth: index out of range"
--  See https://wiki.haskell.org/Avoiding_partial_functions
nth _ = throwStr "invalid call to nth"

first :: Fn
first [Nil               ] = return Nil
first [MalSeq _ _ []     ] = return Nil
first [MalSeq _ _ (x : _)] = return x
first _ = throwStr "illegal call to first"

rest :: Fn
rest [Nil                ] = return $ toList []
rest [MalSeq _ _ []      ] = return $ toList []
rest [MalSeq _ _ (_ : xs)] = return $ toList xs
rest _ = throwStr "illegal call to rest"

empty_Q :: Fn
empty_Q [Nil]           = return $ MalBoolean True
empty_Q [MalSeq _ _ xs] = return $ MalBoolean $ xs == []
empty_Q _               = throwStr "illegal call to empty?"

count :: Fn
count [Nil           ] = return $ MalNumber 0
count [MalSeq _ _ lst] = return $ MalNumber $ length lst
count _ = throwStr "non-sequence passed to count"

concatLast :: [MalVal] -> IOThrows [MalVal]
concatLast [MalSeq _ _ lst] = return lst
concatLast (a : as)         = (a :) <$> concatLast as
concatLast _ = throwStr "last argument of apply must be a sequence"

apply :: Fn
apply (MalFunction _ f : xs) = f =<< concatLast xs
apply _ = throwStr "Illegal call to apply"

do_map :: Fn
do_map [MalFunction _ f, MalSeq _ _ args] = toList <$> mapM (\x -> f [x]) args
do_map _ = throwStr "Illegal call to map"

conj :: Fn
conj (MalSeq _ (Vect False) lst : args) = return $ toList $ reverse args ++ lst
conj (MalSeq _ (Vect True)  lst : args) = return $ MalSeq (MetaData Nil) (Vect True) $ lst ++ args
conj _ = throwStr "illegal arguments to conj"

do_seq :: Fn
do_seq [Nil            ] = return Nil
do_seq [MalSeq _ _ []  ] = return Nil
do_seq [MalSeq _ _ lst ] = return $ toList lst
do_seq [MalString ""   ] = return Nil
do_seq [MalString s    ] = return $ toList $ MalString <$> pure <$> s
do_seq _ = throwStr "seq: called on non-sequence"

-- Metadata functions

with_meta :: Fn
with_meta [MalSeq _ v x,       m] = return $ MalSeq (MetaData m) v x
with_meta [MalHashMap _ x,     m] = return $ MalHashMap (MetaData m) x
with_meta [MalAtom _ x,        m] = return $ MalAtom (MetaData m) x
with_meta [MalFunction _ f,    m] = return $ MalFunction (MetaData m) f
with_meta _ = throwStr "invalid with-meta call"

do_meta :: Fn
do_meta [MalSeq (MetaData m) _ _  ] = return m
do_meta [MalHashMap (MetaData m) _] = return m
do_meta [MalAtom (MetaData m) _   ] = return m
do_meta [MalFunction (MetaData m) _] = return m
do_meta _ = throwStr "invalid meta call"

-- Atom functions

atom :: Fn
atom [val] = MalAtom (MetaData Nil) <$> liftIO (newIORef val)
atom _ = throwStr "invalid atom call"

deref :: Fn
deref [MalAtom _ ref] = liftIO $ readIORef ref
deref _ = throwStr "invalid deref call"

reset_BANG :: Fn
reset_BANG [MalAtom _ ref, val] = do
    liftIO $ writeIORef ref val
    return val
reset_BANG _ = throwStr "invalid reset!"

swap_BANG :: Fn
swap_BANG (MalAtom _ ref : MalFunction _ f : args) = do
    val <- liftIO $ readIORef ref
    new_val <- f (val : args)
    liftIO $ writeIORef ref new_val
    return new_val
swap_BANG _ = throwStr "Illegal swap!"

ns :: [(String, Fn)]
ns = [
    ("=",           equal_Q),
    ("throw",       throw),
    (pred1 "nil?"   nil_Q),
    (pred1 "true?"  true_Q),
    (pred1 "false?" false_Q),
    (pred1 "string?" string_Q),
    ("symbol",      symbol),
    (pred1 "symbol?" symbol_Q),
    ("keyword",     keyword),
    (pred1 "keyword?" keyword_Q),
    (pred1 "number?" number_Q),
    (pred1 "fn?"    fn_Q),
    (pred1 "macro?" macro_Q),

    ("pr-str",      pr_str),
    ("str",         str),
    ("prn",         prn),
    ("println",     println),
    ("readline",    do_readline),
    ("read-string", read_string),
    ("slurp",       slurp),

    (cmp_op "<"     (<)),
    (cmp_op "<="    (<=)),
    (cmp_op ">"     (>)),
    (cmp_op ">="    (>=)),
    (num_op "+"     (+)),
    (num_op "-"     (-)),
    (num_op "*"     (*)),
    (num_op "/"     div),
    ("time-ms",     time_ms),

    ("list",        list),
    (pred1 "list?" list_Q),
    ("vector",      vector),
    (pred1 "vector?" vector_Q),
    ("hash-map",    hash_map),
    (pred1 "map?" map_Q),
    ("assoc",       assoc),
    ("dissoc",      dissoc),
    ("get",         get),
    ("contains?",   contains_Q),
    ("keys",        keys),
    ("vals",        vals),

    (pred1 "sequential?" sequential_Q),
    ("cons",        cons),
    ("concat",      do_concat),
    ("vec",         vec),
    ("nth",         nth),
    ("first",       first),
    ("rest",        rest),
    ("empty?",      empty_Q),
    ("count",       count),
    ("apply",       apply),
    ("map",         do_map),

    ("conj",        conj),
    ("seq",         do_seq),

    ("with-meta",   with_meta),
    ("meta",        do_meta),
    ("atom",        atom),
    (pred1 "atom?" atom_Q),
    ("deref",       deref),
    ("reset!",      reset_BANG),
    ("swap!",       swap_BANG)]
