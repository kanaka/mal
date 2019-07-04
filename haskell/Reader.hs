module Reader
( read_str )
where

import Text.ParserCombinators.Parsec (
    Parser, parse, char, digit, letter, try,
    (<|>), oneOf, noneOf, many, many1, skipMany, skipMany1, sepEndBy, string)
import qualified Data.Map as Map

import Types

spaces :: Parser ()
spaces = skipMany1 (oneOf ", \n")

comment :: Parser ()
comment = char ';' *> skipMany (noneOf "\r\n")

ignored :: Parser ()
ignored = skipMany (spaces <|> comment)

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

escaped :: Parser Char
escaped = f <$> (char '\\' *> oneOf "\\\"n")
    where f 'n' = '\n'
          f x   = x

read_number :: Parser MalVal
read_number = MalNumber . read <$> many1 digit

read_negative_number :: Parser MalVal
read_negative_number = f <$> char '-' <*> many1 digit
    where f sign rest = MalNumber $ read $ sign : rest

read_string :: Parser MalVal
read_string = MalString <$> (char '"' *> many (escaped <|> noneOf "\\\"") <* char '"')

read_symbol :: Parser MalVal
read_symbol = f <$> (letter <|> symbol) <*> many (letter <|> digit <|> symbol)
    where f first rest = g (first : rest)
          g "true"     = MalBoolean True
          g "false"    = MalBoolean False
          g "nil"      = Nil
          g s          = MalSymbol s

read_keyword :: Parser MalVal
read_keyword = MalString . (:) keywordMagic <$> (char ':' *> many (letter <|> digit <|> symbol))

read_atom :: Parser MalVal
read_atom =  read_number
         <|> try read_negative_number
         <|> read_string
         <|> read_keyword
         <|> read_symbol

read_list :: Parser MalVal
read_list = toList <$> (char '(' *> ignored *> sepEndBy read_form ignored <* char ')')

read_vector :: Parser MalVal
read_vector = MalSeq (MetaData Nil) (Vect True) <$> (char '[' *> ignored *> sepEndBy read_form ignored <* char ']')

read_hash_map :: Parser MalVal
read_hash_map = g . keyValuePairs =<< (char '{' *> ignored *> sepEndBy read_form ignored <* char '}')
    where g (Just pairs) = return $ MalHashMap (MetaData Nil) (Map.fromList pairs)
          g Nothing      = fail "invalid contents inside map braces"

-- reader macros
addPrefix :: String -> MalVal -> MalVal
addPrefix s x = toList [MalSymbol s, x]

read_quote :: Parser MalVal
read_quote = addPrefix "quote" <$> (char '\'' *> read_form)

read_quasiquote :: Parser MalVal
read_quasiquote = addPrefix "quasiquote" <$> (char '`' *> read_form)

read_splice_unquote :: Parser MalVal
read_splice_unquote = addPrefix "splice-unquote" <$> (string "~@" *> read_form)

read_unquote :: Parser MalVal
read_unquote = addPrefix "unquote" <$> (char '~' *> read_form)

read_deref :: Parser MalVal
read_deref = addPrefix "deref" <$> (char '@' *> read_form)

read_with_meta :: Parser MalVal
read_with_meta = f <$> (char '^' *> read_form) <*> read_form
    where f m x = toList [MalSymbol "with-meta", x, m]

read_macro :: Parser MalVal
read_macro = read_quote
         <|> read_quasiquote
         <|> try read_splice_unquote <|> read_unquote
         <|> read_deref
         <|> read_with_meta

--

read_form :: Parser MalVal
read_form = ignored *> (
         read_macro
     <|> read_list
     <|> read_vector
     <|> read_hash_map
     <|> read_atom)

read_str :: String -> IOThrows MalVal
read_str str = case parse read_form "Mal" str of
    Left err -> throwStr $ show err
    Right val -> return val
