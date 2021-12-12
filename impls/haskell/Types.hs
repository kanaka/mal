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
            | MalHashMap  MetaData (Map.Map String MalVal)
            | MalAtom     MetaData (IORef MalVal)
            | MalFunction MetaData Fn
            | MalMacro    Fn

--  Stored into maps to distinguish keywords and symbols.
encodeKey :: MalVal -> Maybe String
encodeKey (MalString  s) = pure $ 't' : s
encodeKey (MalKeyword s) = pure $ 'e' : s
encodeKey _              = Nothing

decodeKey :: String -> MalVal
decodeKey ('t' : k) = MalString k
decodeKey ('e' : k) = MalKeyword k
decodeKey _         = error "internal error in Types.decodeKey"

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

kv2map :: Map.Map String MalVal -> [MalVal] -> Maybe MalVal
kv2map start forms = MalHashMap (MetaData Nil) <$> assoc1 start forms where
  assoc1 :: Map.Map String MalVal -> [MalVal] -> Maybe (Map.Map String MalVal)
  assoc1 acc (k : v : kvs) = do
    encoded <- encodeKey k
    assoc1 (Map.insert encoded v acc) kvs
  assoc1 acc [] = Just acc
  assoc1 _ [_] = Nothing
