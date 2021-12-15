module Printer where

import Prelude

import Data.List (List(..), (:))
import Data.Map (toUnfoldable)
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (singleton)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Ref as Ref
import Types (Key(..), MalExpr(..), flatTuples, flatStrings, stringToCharList)



-- PRINT STRING

printStr :: MalExpr -> Effect String
printStr MalNil             = pure "nil"
printStr (MalBoolean b)     = pure $ show b
printStr (MalInt n)         = pure $ show n
printStr (MalTime n)        = pure $ show n
printStr (MalString str)    = pure $ "\"" <> (str # stringToCharList # map unescape # flatStrings) <> "\""
printStr (MalKeyword key)   = pure key
printStr (MalAtom _ r)      = "(atom " <<> (Ref.read r >>= printStr) <>> ")"
printStr (MalSymbol name)   = pure name
printStr (MalList _ xs)     = "(" <<> printList xs <>> ")"
printStr (MalVector _ vs)   = "[" <<> printList vs <>> "]"
printStr (MalHashMap _ hm)  = "{" <<> (hm # toUnfoldable # flatTuples # printList) <>> "}"
printStr (MalFunction _)    = pure "#<function>"


printList :: List MalExpr -> Effect String
printList Nil     = pure ""
printList (x:Nil) = printStr x
printList (x:xs)  = printStr x <> pure " " <> printList xs



-- PRINT STRING READABLY

printStrReadably :: MalExpr -> Effect String
printStrReadably (MalString str)   = pure str
printStrReadably (MalList _ xs)    = "(" <<> printListReadably " " xs <>> ")"
printStrReadably (MalVector _ vs)  = "[" <<> printListReadably " " vs <>> "]"
printStrReadably (MalHashMap _ hm) = "{" <<> (hm # toUnfoldable # flatTuples # printListReadably " ") <>> "}"
printStrReadably ex                = printStr ex


printListReadably :: String -> List MalExpr ->  Effect String
printListReadably _ Nil      = pure ""
printListReadably _ (x:Nil)  = printStrReadably x
printListReadably sep (x:xs) = printStrReadably x <> pure sep <> printListReadably sep xs



-- UTILS

unescape :: Char -> String
unescape '\n' = "\\n"
unescape '\\' = "\\\\"
unescape '"'  = "\\\""
unescape c    = singleton c


keyValuePairs :: List MalExpr -> Maybe (List (Tuple Key MalExpr))
keyValuePairs Nil                      = pure Nil
keyValuePairs (MalString k : v : kvs)  = (:) (Tuple (StringKey k) v) <$> keyValuePairs kvs
keyValuePairs (MalKeyword k : v : kvs) = (:) (Tuple (KeywordKey k) v) <$> keyValuePairs kvs
keyValuePairs _                        = Nothing


leftConcat :: forall m s. Bind m => Applicative m => Semigroup s => s -> m s -> m s
leftConcat op f = (<>) <$> pure op <*> f

infixr 5 leftConcat as <<>


rightConcat :: forall m s. Apply m => Semigroup s => Applicative m => m s -> s -> m s
rightConcat f cl = (<>) <$> f <*> pure cl

infixr 5 rightConcat as <>>