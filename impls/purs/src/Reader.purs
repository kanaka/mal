module Reader (readStr) where

import Prelude

import Control.Alt ((<|>))
import Control.Lazy (fix)
import Data.Either (Either(..))
import Data.Int (fromString)
import Data.List (List(..), many, (:))
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Effect.Exception (throw)
import Printer (keyValuePairs)
import Text.Parsing.Parser (Parser, fail, runParser)
import Text.Parsing.Parser.Combinators (endBy, skipMany, skipMany1, try)
import Text.Parsing.Parser.String (char, noneOf, oneOf, string)
import Text.Parsing.Parser.Token (digit, letter)
import Types (MalExpr(..), charListToString, listToMap, toHashMap, toList, toVector)


spaces :: Parser String Unit
spaces = skipMany1 $ oneOf [',', ' ', '\n']


comment :: Parser String Unit
comment = char ';' *> (skipMany $ noneOf [ '\r', '\n' ])


ignored :: Parser String Unit
ignored = skipMany $ spaces <|> comment


symbol :: Parser String Char
symbol = oneOf ['!', '#', '$', '%', '&', '|', '*', '+', '-', '/', ':', '<', '=', '>', '?', '@', '^', '_', '~']


nat :: Parser String Int
nat = do
  first <- digit
  rest <- many digit
  pure <<< fromMaybe 0 <<< fromString <<< charListToString $ first : rest


escape :: Parser String Char
escape = char '\\'
      *> oneOf ['\\', '\"', 'n']
     <#> case _ of
          'n' -> '\n'
          x   -> x


nonEscape :: Parser String Char
nonEscape =  noneOf [ '\"', '\\' ]



-- ATOM

readAtom :: Parser String MalExpr
readAtom = readNumber
       <|> try readNegativeNumber
       <|> readString
       <|> readKeyword
       <|> readSymbol


readNumber :: Parser String MalExpr
readNumber = MalInt <$> nat


readNegativeNumber :: Parser String MalExpr
readNegativeNumber = MalInt <<< negate <$> (char '-' *> nat)


readString :: Parser String MalExpr
readString = MalString <$> charListToString <$> (char '"' *> many (escape <|> nonEscape) <* char '"')


readKeyword :: Parser String MalExpr
readKeyword =
  MalKeyword <$> charListToString
             <$> ((:) ':')
             <$> (char ':' *> many (letter <|> digit <|> symbol))


readSymbol :: Parser String MalExpr
readSymbol = f <$> (letter <|> symbol) <*> many (letter <|> digit <|> symbol)
  where

  f first rest = charListToString (first:rest)
    # case _ of
      "true"  -> MalBoolean true
      "false" -> MalBoolean false
      "nil"   -> MalNil
      s       -> MalSymbol s



--

readList :: Parser String MalExpr
readList = fix $ \_ ->
  toList <$> (char '(' *> ignored *> endBy readForm ignored <* char ')')



--

readVector :: Parser String MalExpr
readVector = fix $ \_ ->
  toVector <$> (char '[' *> ignored *> endBy readForm ignored <* char ']')



--

readHashMap :: Parser String MalExpr
readHashMap = fix $ \_
  -> char '{' *> ignored *> endBy readForm ignored <* char '}'
  <#> keyValuePairs
  >>= case _ of
    Just ts -> pure $ toHashMap $ listToMap ts
    Nothing -> fail "invalid contents inside map braces"



-- MACROS

readMacro :: Parser String MalExpr
readMacro = fix $ \_ ->
      macro "\'" "quote"
  <|> macro "`" "quasiquote"
  <|> try (macro "~@" "splice-unquote")
  <|> macro "~" "unquote"
  <|> macro "@" "deref"
  <|> readWithMeta


macro :: String -> String -> Parser String MalExpr
macro tok sym = addPrefix sym <$> (string tok *> readForm)
  where

  addPrefix :: String -> MalExpr -> MalExpr
  addPrefix s x = toList $ MalSymbol s : x : Nil


readWithMeta :: Parser String MalExpr
readWithMeta = addPrefix <$> (char '^' *> readForm) <*> readForm
  where

  addPrefix :: MalExpr -> MalExpr -> MalExpr
  addPrefix m x = toList $ MalSymbol "with-meta" : x : m : Nil



--

readForm :: Parser String MalExpr
readForm = fix $ \_ -> ignored
   *> ( readMacro
    <|> readList
    <|> readVector
    <|> readHashMap
    <|> readAtom)



--

readStr :: String -> Effect MalExpr
readStr str = case runParser str readForm of
  Left _    -> throw "EOF"
  Right val -> pure val