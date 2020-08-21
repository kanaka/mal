module Reader
( read_str )
where

import Text.ParserCombinators.Parsec (
    Parser, (<|>), anyChar, char, digit, many, many1, noneOf, oneOf, parse)
import qualified Data.Map as Map

import Types

--  Helpers

symbolFalseTrueNil :: String -> MalVal
symbolFalseTrueNil "true"  = MalBoolean True
symbolFalseTrueNil "false" = MalBoolean False
symbolFalseTrueNil "nil"   = Nil
symbolFalseTrueNil s       = MalSymbol s

addPrefix :: String -> MalVal -> MalVal
addPrefix s m = toList [MalSymbol s, m]

with_meta :: MalVal -> MalVal -> MalVal
with_meta m x = toList [MalSymbol "with-meta", x, m]

hash_map         :: [MalVal] -> Parser MalVal
hash_map = g . keyValuePairs
    where
          g (Just pairs) = return $ MalHashMap (MetaData Nil) $ Map.fromList pairs
          g Nothing      = fail "invalid contents inside map braces"

toKeyword :: String -> MalVal
toKeyword = MalString . (:) keywordMagic

toVector :: [MalVal] -> MalVal
toVector = MalSeq (MetaData Nil) (Vect True)

--  Parsing

--  For efficiency, <|> expects each choice in an alternative to
--  * either succeed,
--  * or fall after looking only at the next character
--  * or consume some input and fail for incorrect input.

--  The grammar should be human-readable in the first and third column
--  without former knowledge of Haskell, except these two regex-style
--  combinators:
--  many  p = (many1 p) | empty     AKA p*, zero or more p
--  many1 p = p (many p)            AKA p+, one or more p

allowedChar    :: Parser Char
allowedChar =                                 noneOf "\n\r \"(),;[\\]{}"

separChar      :: Parser ()
separChar   =  ()                         <$  oneOf "\n ,"
          <|>  ()                         <$  char ';' <* many (noneOf "\n")

sep            :: Parser ()
sep         =  ()                         <$  many separChar
--  A comment may also reach the end of the input. The terminator, if
--  present, will be consumed by the first option later anyway.

escapedChar    :: Parser Char
escapedChar =  '\n'                       <$  char 'n'
          <|>                                 anyChar

stringChar     :: Parser Char
stringChar  =                                 char '\\' *> escapedChar
          <|>                                 noneOf "\""

afterMinus     :: Parser MalVal
afterMinus  =  MalNumber . negate . read  <$> many1 digit
          <|>  MalSymbol . (:) '-'        <$> many allowedChar

afterTilde     :: Parser MalVal
afterTilde  =  addPrefix "splice-unquote" <$> (char '@' *> sep *> form)
          <|>  addPrefix "unquote"        <$> (sep *> form)

form           :: Parser MalVal
form        =  MalString                  <$> (char '"' *> many stringChar <* char '"')
          <|>  addPrefix "quote"          <$> (char '\'' *> sep *> form)
          <|>  toList                     <$> (char '(' *> sep *> many (form <* sep) <* char ')')
          <|>                                 char '-' *> afterMinus
          <|>  MalNumber . read           <$> many1 digit
          <|>  toKeyword                  <$> (char ':' *> many1 allowedChar)
          <|>  addPrefix "deref"          <$> (char '@' *> sep *> form)
          <|>  toVector                   <$> (char '[' *> sep *> many (form <* sep) <* char ']')
          <|>  with_meta                  <$> (char '^' *> sep *> form <* sep) <*> form
          <|>  addPrefix "quasiquote"     <$> (char '`' *> sep *> form)
          <|>  (hash_map                  =<< char '{' *> sep *> many (form <* sep) <* char '}')
          <|>                                 char '~' *> afterTilde
          <|>  symbolFalseTrueNil         <$> many1 allowedChar

top            :: Parser MalVal
top         =                                 sep *> form

read_str :: String -> IOThrows MalVal
read_str str = case parse top "Mal" str of
    Left err -> throwStr $ show err
    Right val -> return val
