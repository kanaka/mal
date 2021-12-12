module Reader
( read_str )
where

import qualified Data.Map.Strict as Map
import Text.ParserCombinators.Parsec (
    Parser, parse, char, digit, anyChar,
    (<|>), oneOf, noneOf, many, many1)

import Types

----------------------------------------------------------------------
--  A MAL grammar and a possible parsing are described here.

--  If you are only interested in the grammar, please ignore the
--  left-hand side of <$> and =<< operators (second column).

--  *>  <*  <*>                     all mean concatenation
--  <|>                             means alternative
--  many  p = (many1 p) | empty     means p*, zero or more p
--  many1 p = p (many p)            means p+, one or more p

--  For efficiency, the alternative operator <|> expects each branch
--  to either:
--  * succeed,
--  * fall after looking at the next character without consuming it,
--  * or consume some input and fail, indicating that the input is
--    incorrect and no remaining branches should be ignored.

allowedChar   :: Parser Char
allowedChar =                    noneOf "\n\r \"(),;[\\]{}"

sep           :: Parser String
sep         =                    many (oneOf  ", \n"
                                       <|> char ';' <* many (noneOf "\n"))

stringChar    :: Parser Char
stringChar  = unescapeChar  <$> (char '\\' *>  anyChar)
          <|>                    noneOf "\""

afterMinus    :: Parser MalVal
afterMinus  = negative      <$>  many1 digit
          <|> hyphenSymbol  <$>  many allowedChar

afterTilde    :: Parser MalVal
afterTilde  = spliceUnquote <$> (char '@' *> sep *> form)
          <|> unquote       <$> (sep *> form)

form          :: Parser MalVal
form        = MalString     <$> (char '"'  *> many stringChar <* char '"')
          <|> MalKeyword    <$> (char ':'  *> many1 allowedChar)
          <|>                    char '-'  *> afterMinus
          <|> toList        <$> (char '('  *> sep *> many (form <* sep) <* char ')')
          <|> vector        <$> (char '['  *> sep *> many (form <* sep) <* char ']')
          <|> (toMap        =<<  char '{'  *> sep *> many (form <* sep) <* char '}')
          <|> quote         <$> (char '\'' *> sep *> form)
          <|> quasiquote    <$> (char '`'  *> sep *> form)
          <|> deref         <$> (char '@'  *> sep *> form)
          <|>                    char '~'  *> afterTilde
          <|> withMeta      <$> (char '^'  *> sep *> form <* sep) <*> form
          <|> positive      <$>  many1 digit
          <|> symbol        <$>  many1 allowedChar

read_form     :: Parser MalVal
read_form   =                    sep *> form

----------------------------------------------------------------------
--  Part specific to Haskell

addPrefix :: String -> MalVal -> MalVal
addPrefix s x = toList [MalSymbol s, x]

deref :: MalVal -> MalVal
deref = addPrefix "deref"

hyphenSymbol :: String -> MalVal
hyphenSymbol = MalSymbol . (:) '-'

negative ::  String ->  MalVal
negative = MalNumber . negate . read

positive :: String -> MalVal
positive = MalNumber . read

quasiquote :: MalVal -> MalVal
quasiquote = addPrefix "quasiquote"

quote :: MalVal -> MalVal
quote = addPrefix "quote"

spliceUnquote :: MalVal -> MalVal
spliceUnquote = addPrefix "splice-unquote"

toMap :: [MalVal] -> Parser MalVal
toMap kvs = case kv2map Map.empty kvs of
  Just m -> return m
  Nothing -> fail "invalid contents in map braces"

unquote :: MalVal -> MalVal
unquote = addPrefix "unquote"

symbol ::  String -> MalVal
symbol "true"  = MalBoolean True
symbol "false" = MalBoolean False
symbol "nil"   = Nil
symbol s       = MalSymbol s

unescapeChar :: Char -> Char
unescapeChar 'n' = '\n'
unescapeChar c   = c

vector :: [MalVal] -> MalVal
vector = MalSeq (MetaData Nil) (Vect True)

withMeta :: MalVal -> MalVal -> MalVal
withMeta m d = toList [MalSymbol "with-meta", d, m]

--  The only exported function

read_str :: String -> IOThrows MalVal
read_str str = case parse read_form "Mal" str of
    Left err -> throwStr $ show err
    Right val -> return val
