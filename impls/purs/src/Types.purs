module Types where

import Prelude

import Data.Array as Array
import Data.Foldable (class Foldable)
import Data.List (List(..), foldr, (:))
import Data.List as List
import Data.Map (Map)
import Data.Map.Internal as Map
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.Traversable (foldl)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Ref (Ref)
import Effect.Ref as Ref


data MalExpr
  = MalNil
  | MalBoolean Boolean
  | MalInt Int
  | MalTime Time
  | MalString String
  | MalKeyword String
  | MalSymbol String
  | MalAtom Meta (Ref MalExpr)
  | MalList Meta (List MalExpr)
  | MalVector Meta (List MalExpr)
  | MalHashMap Meta (Map Key MalExpr)
  | MalFunction { fn     :: MalFn
                , ast    :: MalExpr
                , env    :: RefEnv
                , params :: List String
                , macro  :: Boolean
                , meta   :: MalExpr
                }

type Time = Number


instance Eq MalExpr where
  eq MalNil MalNil                     = true
  eq (MalBoolean a) (MalBoolean b)     = a == b
  eq (MalInt a) (MalInt b)             = a == b
  eq (MalTime a) (MalTime b)           = a == b
  eq (MalString a) (MalString b)       = a == b
  eq (MalKeyword a) (MalKeyword b)     = a == b
  eq (MalSymbol a) (MalSymbol b)       = a == b

  eq (MalList _ a) (MalList _ b)       = a == b
  eq (MalVector _ a) (MalList _ b)     = a == b
  eq (MalList _ a) (MalVector _ b)     = a == b

  eq (MalVector _ a) (MalVector _ b)   = a == b
  eq (MalHashMap _ a) (MalHashMap _ b) = a == b
  eq _ _                               = false


data Key = StringKey String
         | KeywordKey String

derive instance Eq Key
derive instance Ord Key


type MalFn = List MalExpr -> Effect MalExpr


type Local = Map String MalExpr
type RefEnv = List (Ref.Ref Local)



-- Metas

newtype Meta = Meta MalExpr


toList :: List MalExpr -> MalExpr
toList = MalList (Meta MalNil)


toVector :: List MalExpr -> MalExpr
toVector = MalVector (Meta MalNil)


toAtom :: Ref MalExpr -> MalExpr
toAtom = MalAtom (Meta MalNil)


toHashMap :: Map Key MalExpr -> MalExpr
toHashMap = MalHashMap (Meta MalNil)



-- Utils

listToMap :: List (Tuple Key MalExpr) -> Map Key MalExpr
listToMap = Map.fromFoldable


charListToString :: List Char -> String
charListToString = fromCharArray <<< Array.fromFoldable


stringToCharList :: String -> List Char
stringToCharList = List.fromFoldable <<< toCharArray


flatStrings :: List String -> String
flatStrings = foldr (<>) ""


flatTuples :: List (Tuple Key MalExpr) -> List MalExpr
flatTuples ((Tuple (StringKey a) b) : xs)  = MalString a : b : flatTuples xs
flatTuples ((Tuple (KeywordKey a) b) : xs) = MalKeyword a : b : flatTuples xs
flatTuples _                               = Nil


foldrM :: forall a m b f. Foldable f => Monad m => (a -> b -> m b) -> b -> f a -> m b
foldrM f z0 xs = foldl c pure xs z0
  where c k x z = f x z >>= k


keyToString :: Key -> MalExpr
keyToString (StringKey k)  = MalString k
keyToString (KeywordKey k) = MalKeyword k


keyValuePairs :: List MalExpr -> Maybe (List (Tuple String MalExpr))
keyValuePairs Nil                     = pure Nil
keyValuePairs (MalString k : v : kvs) = (:)  (Tuple k v) <$> keyValuePairs kvs
keyValuePairs _                       = Nothing