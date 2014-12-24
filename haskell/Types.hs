module Types
(MalVal (..), Fn (..), EnvData (..), Env,
 catchAny, _pairs, _func, _malfunc, _list_Q, _vector_Q, _hash_map_Q)
where

import Data.IORef (IORef)
import qualified Data.Map as Map
import Control.Exception as CE


-- Base Mal types --
newtype Fn = Fn ([MalVal] -> IO MalVal)
data MalVal = Nil
            | MalFalse
            | MalTrue
            | MalNumber   Int
            | MalString   String
            | MalSymbol   String
            | MalKeyword  String
            | MalList     [MalVal]
            | MalVector   [MalVal]
            | MalHashMap  (Map.Map String MalVal)
            | Func         Fn
            | MalFunc      {fn :: Fn,
                            ast :: MalVal,
                            env :: Env,
                            params :: MalVal,
                            macro :: Bool}

_equal_Q Nil Nil = True
_equal_Q MalFalse MalFalse = True
_equal_Q MalTrue MalTrue = True
_equal_Q (MalNumber a) (MalNumber b) = a == b
_equal_Q (MalString a) (MalString b) = a == b
_equal_Q (MalSymbol a) (MalSymbol b) = a == b
_equal_Q (MalKeyword a) (MalKeyword b) = a == b
_equal_Q (MalList a) (MalList b) = a == b
_equal_Q (MalList a) (MalVector b) = a == b
_equal_Q (MalVector a) (MalList b) = a == b
_equal_Q (MalHashMap a) (MalHashMap b) = a == b
_equal_Q _ _ = False

instance Eq MalVal where
    x == y = _equal_Q x y


-- Env types --
-- Note: Env functions are in Env module
data EnvData = EnvPair (Maybe Env, (Map.Map String MalVal))
type Env = IORef EnvData



----------------------------------------------------------

-- General functions --

_obj_type :: MalVal     -> String
_obj_type (Nil)          = "nil"
_obj_type (MalFalse)     = "false"
_obj_type (MalTrue)      = "true"
_obj_type (MalNumber _)  = "number"
_obj_type (MalString _)  = "string"
_obj_type (MalSymbol _)  = "symbol"
_obj_type (MalList _)    = "list"
_obj_type (MalVector _)  = "vector"
_obj_type (MalHashMap _) = "hashmap"
_obj_type (Func _)       = "function"

-- TODO: propagate error properly
_pairs [x] = error "Odd number of elements to _pairs"
_pairs [] = []
_pairs (MalString x:y:xs) = (x,y):_pairs xs
_pairs (MalKeyword x:y:xs) = ("\x029e" ++ x,y):_pairs xs

-- Errors

catchAny :: IO a -> (CE.SomeException -> IO a) -> IO a
catchAny = CE.catch


-- Functions

_func fn = Func $ Fn fn
_malfunc ast env params fn = MalFunc {fn=(Fn fn), ast=ast,
                                      env=env, params=params,
                                      macro=False}

-- Lists

_list_Q (MalList _) = MalTrue
_list_Q _           = MalFalse

-- Vectors

_vector_Q (MalVector _) = MalTrue
_vector_Q _             = MalFalse

-- Hash Maps

_hash_map_Q (MalHashMap _) = MalTrue
_hash_map_Q _              = MalFalse

