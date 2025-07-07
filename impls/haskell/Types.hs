module Types
( MalVal (..), IOThrows, Fn, MetaData (..), Vect (..),
  decodeKey, encodeKey, kv2map,
  throwStr, toList)
where

import Data.IORef (IORef)
import qualified Data.Map.Strict as Map
--  The documentation recommends strict except in specific cases.
import Control.Monad.Except (ExceptT, throwError)

-- Base Mal types --
type Fn = [MalVal] -> IOThrows MalVal

-- Use type safety for unnamed components, without runtime penalty.
newtype MetaData = MetaData MalVal
newtype Vect     = Vect     Bool

data MalVal = Nil
            | MalBoolean  Bool
            | MalNumber   Int
            | MalString   String
            | MalSymbol   String
            | MalKeyword  String
            | MalSeq      MetaData Vect [MalVal]
            | MalHashMap  MetaData (Map.Map MapKey MalVal)
            | MalAtom     MetaData (IORef MalVal)
            | MalFunction MetaData Fn
            | MalMacro    Fn

--  Stored into maps to distinguish keywords and symbols.
--  MapKey is not exported, other modules use encodeKey or kv2map.
data MapKey = MapKeyKeyword String | MapKeyString String
instance Eq MapKey where
    MapKeyString  a == MapKeyString  b = a == b
    MapKeyKeyword a == MapKeyKeyword b = a == b
    _               == _               = False
instance Ord MapKey where
    compare (MapKeyString  a) (MapKeyString  b) = compare a b
    compare (MapKeyKeyword a) (MapKeyKeyword b) = compare a b
    compare (MapKeyKeyword _) (MapKeyString  _) = LT
    compare (MapKeyString  _) (MapKeyKeyword _) = GT

encodeKey :: MalVal -> IOThrows MapKey
encodeKey (MalString  key) = pure $ MapKeyString  key
encodeKey (MalKeyword key) = pure $ MapKeyKeyword key
encodeKey _ = throwStr "map keys must be keywords or strings"

decodeKey :: MapKey -> MalVal
decodeKey (MapKeyString  k) = MalString  k
decodeKey (MapKeyKeyword k) = MalKeyword k

instance Eq MalVal where
    Nil              == Nil              = True
    (MalBoolean a)   == (MalBoolean b)   = a == b
    (MalNumber a)    == (MalNumber b)    = a == b
    (MalString a)    == (MalString b)    = a == b
    (MalKeyword a)   == (MalKeyword b)   = a == b
    (MalSymbol a)    == (MalSymbol b)    = a == b
    (MalSeq _ _ a)   == (MalSeq _ _ b)   = a == b
    (MalHashMap _ a) == (MalHashMap _ b) = a == b
    (MalAtom _ a)    == (MalAtom _ b)    = a == b
    _                == _                = False

--- Errors/Exceptions ---

type IOThrows = ExceptT MalVal IO

throwStr :: String -> IOThrows a
throwStr = throwError . MalString

--  Convenient shortcuts for common situations.

toList :: [MalVal] -> MalVal
toList = MalSeq (MetaData Nil) (Vect False)

--  Use Maybe because Core throws while Reader fails.
kv2map :: Map.Map MapKey MalVal -> [MalVal] -> Maybe MalVal
kv2map start forms = MalHashMap (MetaData Nil) <$> assoc1 start forms where
  assoc1 :: Map.Map MapKey MalVal -> [MalVal] -> Maybe (Map.Map MapKey MalVal)
  assoc1 acc (MalKeyword s : v : kvs) = assoc1 (Map.insert (MapKeyKeyword s) v acc) kvs
  assoc1 acc (MalString  s : v : kvs) = assoc1 (Map.insert (MapKeyString  s) v acc) kvs
  assoc1 acc [] = Just acc
  assoc1 _ _ = Nothing
