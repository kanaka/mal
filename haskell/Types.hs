module Types
--( MalVal (Nil,MalFalse,MalTrue,MalNumber,MalString,MalSymbol,MalKeyword,MalList,MalVector,MalFunc), _obj_type )
(MalVal (..), FuncT (..), _malfunc, catchAny)
where

import qualified Data.Map as Map
import Control.Exception (SomeException, catch)

-- Based Mal types --
newtype FuncT = FuncT (MalVal -> MalVal)
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
            | MalFunc     FuncT
            deriving (Eq)

instance Eq FuncT where
    x == y = False

_malfunc f = MalFunc $ FuncT f


-- Error definitions --
catchAny :: IO a -> (SomeException -> IO a) -> IO a
catchAny = catch


----------------------------------------------------------

-- General type functions --

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
_obj_type (MalFunc _)    = "malfunc"
