module Types
( MalVal (..), IOThrows, Fn, Env, MetaData (..), Vect (..),
  keyValuePairs, throwStr, toList, keywordMagic)
where

import Data.IORef (IORef)
import qualified Data.Map as Map
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
            | MalSeq      MetaData Vect [MalVal]
            | MalHashMap  MetaData (Map.Map String MalVal)
            | MalAtom     MetaData (IORef MalVal)
            | MalFunction {fn :: Fn,
                           f_ast :: MalVal,
                           f_params :: [String],
                           macro :: Bool,
                           meta :: MalVal}

keywordMagic :: Char
keywordMagic = '\x029e'

_equal_Q :: MalVal -> MalVal -> Bool
_equal_Q Nil Nil = True
_equal_Q (MalBoolean a) (MalBoolean b) = a == b
_equal_Q (MalNumber a) (MalNumber b) = a == b
_equal_Q (MalString a) (MalString b) = a == b
_equal_Q (MalSymbol a) (MalSymbol b) = a == b
_equal_Q (MalSeq _ _ a) (MalSeq _ _ b) = a == b
_equal_Q (MalHashMap _ a) (MalHashMap _ b) = a == b
_equal_Q (MalAtom _ a) (MalAtom _ b) = a == b
_equal_Q _ _ = False

instance Eq MalVal where
    x == y = _equal_Q x y


--- Errors/Exceptions ---

type IOThrows = ExceptT MalVal IO

throwStr :: String -> IOThrows a
throwStr = throwError . MalString

-- Env types --
-- Note: Env functions are in Env module
type Env = [IORef (Map.Map String MalVal)]

--  Convenient shortcuts for common situations.

toList :: [MalVal] -> MalVal
toList = MalSeq (MetaData Nil) (Vect False)

keyValuePairs :: [MalVal] -> Maybe [(String, MalVal)]
keyValuePairs []                      = pure []
keyValuePairs (MalString k : v : kvs) = ((k, v) :) <$> keyValuePairs kvs
keyValuePairs _ = Nothing
