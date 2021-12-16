module Core (ns) where

import Prelude

import Data.DateTime.Instant (unInstant)
import Data.Int (ceil, toNumber)
import Data.List (List(..), concat, drop, foldM, fromFoldable, length, reverse, (:))
import Data.Map.Internal as Map
import Data.Maybe (Maybe(..))
import Data.String (take)
import Data.String.CodeUnits (singleton)
import Data.Time.Duration (Milliseconds(..), toDuration)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Exception (throw)
import Effect.Now (now)
import Effect.Ref as Ref
import Reader (readStr)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Printer (keyValuePairs, printList, printListReadably, printStrReadably)
import Readline (readLine)
import Types (Key(..), MalExpr(..), MalFn, Meta(..), keyToString, stringToCharList, toAtom, toHashMap, toList, toVector)



ns :: List (Tuple String MalFn)
ns = fromFoldable
  [ Tuple "throw"      throw'

  , Tuple "true?"       $ pred1 trueQ
  , Tuple "false?"      $ pred1 falseQ

  , Tuple "="           eqQ
  , Tuple "+"           $ numOp (+)
  , Tuple "-"           $ numOp (-)
  , Tuple "*"           $ numOp (*)
  , Tuple "/"           $ numOp (/)
  , Tuple "<"           $ cmpOp (<)
  , Tuple "<="          $ cmpOp (<=)
  , Tuple ">"           $ cmpOp (>)
  , Tuple ">="          $ cmpOp (>=)
  , Tuple "number?"     $ pred1 numberQ

  , Tuple "pr-str"      prStr
  , Tuple "str"         str
  , Tuple "string?"     $ pred1 stringQ
  , Tuple "prn"         prn
  , Tuple "println"     println
  , Tuple "slurp"       slurp
  , Tuple "readline"    readline'
  , Tuple "read-string" readString
  , Tuple "time-ms"     timeMs

  , Tuple "symbol?"     $ pred1 symbolQ
  , Tuple "symbol"      symbol
  , Tuple "keyword?"    $ pred1 keywordQ
  , Tuple "keyword"     keyword

  , Tuple "list"        list
  , Tuple "list?"       $ pred1 listQ
  , Tuple "nil?"        $ pred1 nilQ
  , Tuple "empty?"      $ pred1 emptyQ
  , Tuple "count"       count
  , Tuple "sequential?" $ pred1 sequentialQ
  , Tuple "cons"        cons
  , Tuple "concat"      concat'
  , Tuple "nth"         nth
  , Tuple "first"       first
  , Tuple "rest"        rest
  , Tuple "apply"       apply'
  , Tuple "map"         map'
  , Tuple "map?"        $ pred1 mapQ
  , Tuple "conj"        conj'
  , Tuple "seq"         seq

  , Tuple "vec"         vec
  , Tuple "vector"      vector
  , Tuple "vector?"     $ pred1 vectorQ

  , Tuple "hash-map"    hashMap
  , Tuple "assoc"       assoc
  , Tuple "dissoc"      dissoc
  , Tuple "get"         get
  , Tuple "contains?"   containsQ
  , Tuple "keys"        keys
  , Tuple "vals"        vals

  , Tuple "meta"        meta
  , Tuple "with-meta"   withMeta

  , Tuple "atom"        atom
  , Tuple "atom?"       $ pred1 atomQ
  , Tuple "deref"       deref
  , Tuple "reset!"      resetB
  , Tuple "swap!"       swapB

  , Tuple "macro?"      $ pred1 macroQ

  , Tuple "fn?"         $ pred1 fnQ
  ]



-- General functions

eqQ :: MalFn
eqQ (a:b:Nil) = pure $ MalBoolean $ a == b
eqQ _         = throw "illegal arguments to ="



-- Error/Exception functions

throw' :: MalFn
throw' (e:Nil) = throw =<< printStrReadably e
throw' _       = throw "illegal arguments to throw"



-- Boolean functions

trueQ :: MalExpr -> Boolean
trueQ (MalBoolean true) = true
trueQ _                 = false


falseQ :: MalExpr -> Boolean
falseQ (MalBoolean false) = true
falseQ _                  = false


-- Numeric functions

numOp ∷  (Number → Number → Number) → MalFn
numOp op (MalInt n1 : MalInt n2 : Nil)   = pure $ MalInt $ ceil $ op (toNumber n1) (toNumber n2)
numOp op (MalInt n1 : MalTime n2 : Nil)  = pure $ MalInt $ ceil $ op (toNumber n1) n2
numOp op (MalTime n1 : MalInt n2 : Nil)  = pure $ MalInt $ ceil $ op n1 (toNumber n2)
numOp op (MalTime n1 : MalTime n2 : Nil) = pure $ MalTime $ op n1 n2
numOp _ _                                = throw "invalid operator"


cmpOp ∷  (Number → Number → Boolean) → List MalExpr → Effect MalExpr
cmpOp op (MalInt n1 : MalInt n2 : Nil)   = pure $ MalBoolean $ op (toNumber n1) (toNumber n2)
cmpOp op (MalInt n1 : MalTime n2 : Nil)  = pure $ MalBoolean $ op (toNumber n1) n2
cmpOp op (MalTime n1 : MalInt n2 : Nil)  = pure $ MalBoolean $ op n1 (toNumber n2)
cmpOp op (MalTime n1 : MalTime n2 : Nil) = pure $ MalBoolean $ op n1 n2
cmpOp _ _                                = throw "invalid operator"


numberQ :: MalExpr -> Boolean
numberQ (MalInt _)  = true
numberQ (MalTime _) = true
numberQ _           = false



-- String functions

prStr :: MalFn
prStr a = liftEffect $ MalString <$> printList a


str :: MalFn
str a = liftEffect $ MalString <$> printListReadably "" a


stringQ :: MalExpr -> Boolean
stringQ (MalString "") = true
stringQ (MalString s)  = take 1 s /= ":"
stringQ _              = false


prn :: MalFn
prn args = liftEffect $ do
  log =<< printList args
  pure MalNil


println :: MalFn
println args = liftEffect $ do
  log =<< printListReadably " " args
  pure MalNil


slurp :: MalFn
slurp (MalString path : Nil) = MalString <$> liftEffect (readTextFile UTF8 path)
slurp _                      = throw "invalid arguments to slurp"


readline' :: MalFn
readline' (MalString prompt : Nil) = MalString <$> readLine prompt
readline' _                        = throw "invalid arguments to readline"


readString :: MalFn
readString (MalString s : Nil) = readStr s
readString _                   = throw "invalid read-string"


timeMs :: MalFn
timeMs Nil = do
  n <- now
  pure $ MalTime $ (unwap <<< toDuration <<< unInstant) n
  where

  unwap :: Milliseconds -> Number
  unwap (Milliseconds n) = n

timeMs _ = throw "invalid time-ms"



-- Scalar functions

symbolQ :: MalExpr -> Boolean
symbolQ (MalSymbol _) = true
symbolQ _             = false


symbol :: MalFn
symbol (MalString s : Nil) = pure $ MalSymbol s
symbol _                   = throw "symbol called with non-string"


keywordQ :: MalExpr -> Boolean
keywordQ (MalKeyword s) = take 1 s == ":"
keywordQ _             = false


keyword :: MalFn
keyword (kw@(MalString s) : Nil) | take 1 s == ":" = pure kw
keyword (MalString s : Nil)  = pure $ MalKeyword (":" <> s)
keyword (kw@(MalKeyword s) : Nil) | take 1 s == ":" = pure kw
keyword (MalKeyword s : Nil) = pure $ MalKeyword (":" <> s)
keyword _                    = throw "keyword called with non-string"



-- List functions

list :: MalFn
list = pure <<< toList


listQ :: MalExpr -> Boolean
listQ (MalList _ _ ) = true
listQ _              = false


nilQ :: MalExpr -> Boolean
nilQ MalNil = true
nilQ _      = false


emptyQ :: MalExpr -> Boolean
emptyQ (MalList _ Nil)   = true
emptyQ (MalVector _ Nil) = true
emptyQ _                 = false


count :: MalFn
count (MalNil:Nil)           = pure $ MalInt 0
count (MalList _ ex : Nil)   = pure $ MalInt $ length ex
count (MalVector _ ex : Nil) = pure $ MalInt $ length ex
count _                      = throw "non-sequence passed to count"


sequentialQ :: MalExpr -> Boolean
sequentialQ (MalList _ _)   = true
sequentialQ (MalVector _ _) = true
sequentialQ _               = false


cons :: MalFn
cons (x:Nil)                    = pure $ toList $ x:Nil
cons (x : MalList _ xs : Nil)   = pure $ toList $ x:xs
cons (x : MalVector _ xs : Nil) = pure $ toList $ x:xs
cons _                          = throw "illegal call to cons"


concat' :: MalFn
concat' args = toList <<< concat <$> traverse unwrapSeq args
  where

  unwrapSeq :: MalExpr -> Effect (List MalExpr)
  unwrapSeq (MalList _ xs)   = pure xs
  unwrapSeq (MalVector _ xs) = pure xs
  unwrapSeq _                = throw "invalid concat"


nth :: MalFn
nth (MalList _ xs : MalInt n : Nil)   =
  case drop n xs of
    x:_ -> pure x
    Nil -> throw "nth: index out of range"
nth (MalVector _ xs : MalInt n : Nil) =
  case drop n xs of
    x:_ -> pure x
    Nil -> throw "nth: index out of range"
nth _                                 = throw "invalid call to nth"


first :: MalFn
first (MalNil:Nil)              = pure MalNil
first (MalList _ Nil : Nil)     = pure MalNil
first (MalList _ (x:_) : Nil)   = pure x
first (MalVector _ Nil : Nil)   = pure MalNil
first (MalVector _ (x:_) : Nil) = pure x
first _                         = throw "illegal call to first"


rest :: MalFn
rest (MalNil:Nil)               = pure $ toList Nil
rest (MalList _ Nil : Nil)      = pure $ toList Nil
rest (MalList _ (_:xs) : Nil)   = pure $ toList xs
rest (MalVector _ Nil : Nil)    = pure $ toList Nil
rest (MalVector _ (_:xs) : Nil) = pure $ toList xs
rest _                          = throw "illegal call to rest"


apply' :: MalFn
apply' (MalFunction {fn:f} : as) = f =<< concatLast as
  where
  concatLast :: List MalExpr -> Effect (List MalExpr)
  concatLast (MalList _ lst : Nil)   = pure lst
  concatLast (MalVector _ lst : Nil) = pure lst
  concatLast (x:xs)                  = (:) x <$> concatLast xs
  concatLast _                       = throw "last argument of apply must be a sequence"
apply' _ = throw "Illegal call to apply"


map' :: MalFn
map' (MalFunction {fn:f} : MalList _ args : Nil)   = toList <$> traverse (\x -> f (x:Nil)) args
map' (MalFunction {fn:f} : MalVector _ args : Nil) = toList <$> traverse (\x -> f (x:Nil)) args
map' _ = throw "Illegal call to map"


mapQ :: MalExpr -> Boolean
mapQ (MalHashMap _ _) = true
mapQ _                = false


conj' :: MalFn
conj' (MalList _ es : args)   = pure $ toList $ reverse args <> es
conj' (MalVector _ es : args) = pure $ toVector $ es <> args
conj' _                       = throw "illegal arguments to conj"


seq :: MalFn
seq (MalNil:Nil)            = pure MalNil
seq (MalList _ Nil : Nil)   = pure MalNil
seq (MalList _ es : Nil)    = pure $ toList es
seq (MalVector _ Nil : Nil) = pure MalNil
seq (MalVector _ es : Nil)  = pure $ toList es
seq (MalString "" : Nil)    = pure MalNil
seq (MalString s : Nil)     = pure $ toList $ map (MalString <<< singleton) (stringToCharList s)
seq _                       = throw "seq: called on non-sequence"



-- Vector functions

vec :: MalFn
vec (MalList _ xs : Nil)   = pure $ toVector xs
vec (MalVector _ xs : Nil) = pure $ toVector xs
vec Nil                  = throw "vec: arg type"
vec _                    = throw "vec: arg type"


vector :: MalFn
vector = pure <<< toVector


vectorQ :: MalExpr -> Boolean
vectorQ (MalVector _ _) = true
vectorQ _               = false



-- Hash Map functions

hashMap :: MalFn
hashMap kvs =
  case keyValuePairs kvs of
    Just pairs -> pure $ toHashMap $ Map.fromFoldable pairs
    Nothing    -> throw "invalid call to hash-map"


assoc :: MalFn
assoc (MalHashMap _ hm : kvs) =
    case keyValuePairs kvs of
        Just pairs -> pure $ toHashMap $ Map.union (Map.fromFoldable pairs) hm
        Nothing    -> throw "invalid assoc"
assoc _                       = throw "invalid call to assoc"


dissoc :: MalFn
dissoc (MalHashMap _ hm : ks) = toHashMap <$> foldM remover hm ks
  where
  remover :: Map.Map Key MalExpr -> MalExpr -> Effect (Map.Map Key MalExpr)
  remover m (MalKeyword k) = pure $ Map.delete (KeywordKey k) m
  remover m (MalString k)  = pure $ Map.delete (StringKey k) m
  remover _ _              = throw "invalid dissoc"
dissoc _                      = throw "invalid call to dissoc"


get :: MalFn
get (MalHashMap _ hm : MalString k : Nil)  =
  pure case Map.lookup (StringKey k) hm of
    Just mv -> mv
    Nothing -> MalNil
get (MalHashMap _ hm : MalKeyword k : Nil) =
  pure case Map.lookup (KeywordKey k) hm of
    Just mv -> mv
    Nothing -> MalNil
get (MalNil : MalString _ : Nil)           = pure MalNil
get _                                      = throw "invalid call to get"


containsQ :: MalFn
containsQ (MalHashMap _ hm : MalString k : Nil)  = pure $ MalBoolean $ Map.member (StringKey k) hm
containsQ (MalHashMap _ hm : MalKeyword k : Nil) = pure $ MalBoolean $ Map.member (KeywordKey k) hm
containsQ (MalNil : MalString _ : Nil)           = pure $ MalBoolean false
containsQ _                                      = throw "invalid call to contains?"


keys :: MalFn
keys (MalHashMap _ hm : Nil) = pure $ toList $ keyToString <$> Map.keys hm
keys _                     = throw "invalid call to keys"


vals :: MalFn
vals (MalHashMap _ hm : Nil) = pure $ toList $ Map.values hm
vals _                       = throw "invalid call to vals"



-- Metadata functions

meta :: MalFn
meta (MalList (Meta m) _ : Nil)    = pure m
meta (MalVector (Meta m) _ : Nil)  = pure m
meta (MalHashMap (Meta m) _ : Nil) = pure m
meta (MalAtom (Meta m) _ : Nil)    = pure m
meta (MalFunction {meta:m} : Nil)  = pure m
meta _                             = throw "invalid meta call"


withMeta :: MalFn
withMeta (MalList _ es : m : Nil)    = pure $ MalList (Meta m) es
withMeta (MalVector _ es : m : Nil)  = pure $ MalVector (Meta m) es
withMeta (MalHashMap _ es : m : Nil) = pure $ MalHashMap (Meta m) es
withMeta (MalAtom _ es : m : Nil)    = pure $ MalAtom (Meta m) es
withMeta ((MalFunction f) : m : Nil) = pure $ MalFunction $ f {meta = m}
withMeta _                           = throw "invalid with-meta call"



-- Atom functions

atom :: MalFn
atom (v:Nil) = toAtom <$> liftEffect (Ref.new v)
atom _       = throw "invalid atom call"


atomQ :: MalExpr -> Boolean
atomQ (MalAtom _ _) = true
atomQ _             = false


deref :: MalFn
deref (MalAtom _ ref : Nil) = liftEffect $ Ref.read ref
deref _                   = throw "invalid deref call"


resetB :: MalFn
resetB (MalAtom _ ref : val : Nil) = liftEffect $ Ref.write val ref *> pure val
resetB _                         = throw "invalid reset!"


swapB :: MalFn
swapB (MalAtom _ ref : MalFunction {fn:f} : args) = do
  val <- liftEffect $ Ref.read ref
  newVal <- f $ val:args
  liftEffect $ Ref.write newVal ref
  pure newVal
swapB _                                           = throw "Illegal swap!"



-- Macro

macroQ :: MalExpr -> Boolean
macroQ (MalFunction {macro:true}) = true
macroQ _                          = false



-- Function

fnQ :: MalExpr -> Boolean
fnQ (MalFunction {macro:false}) = true
fnQ _                           = false



-- Utils

pred1 :: (MalExpr -> Boolean) -> MalFn
pred1 f (x:Nil) = pure $ MalBoolean $ f x
pred1 _ _       = throw "illegal call to unary predicate"