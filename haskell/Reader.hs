module Reader
( read_str )
where

import Text.ParserCombinators.Parsec (
    Parser, parse, space, char, digit, letter, try,
    (<|>), oneOf, noneOf, many, many1, skipMany, skipMany1, sepEndBy)
import qualified Data.Map as Map

import Types

spaces :: Parser ()
spaces = skipMany1 (oneOf ", \n")

comment :: Parser ()
comment = do
    char ';'
    skipMany (noneOf "\r\n")

ignored :: Parser ()
ignored = skipMany (spaces <|> comment)

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

escaped :: Parser Char
escaped = do
    char '\\'
    x <- oneOf "\\\"n"
    case x of
        'n' -> return '\n'
        _   -> return x

read_number :: Parser MalVal
read_number = do
    x <- many1 digit
    return $ MalNumber $ read x

read_negative_number :: Parser MalVal
read_negative_number = do
    sign <- char '-'
    rest <- many1 digit
    return $ MalNumber $ read $ sign:rest

read_string :: Parser MalVal
read_string = do
    char '"'
    x <- many (escaped <|> noneOf "\\\"")
    char '"'
    return $ MalString x

read_symbol :: Parser MalVal
read_symbol = do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    let str = first:rest
    return $ case str of
        "true"  -> MalTrue
        "false" -> MalFalse
        "nil"   -> Nil
        _       -> MalSymbol str

read_keyword :: Parser MalVal
read_keyword = do
    char ':'
    x <- many (letter <|> digit <|> symbol)
    return $ MalString $ "\x029e" ++ x

read_atom :: Parser MalVal
read_atom =  read_number
         <|> try read_negative_number
         <|> read_string
         <|> read_keyword
         <|> read_symbol

read_list :: Parser MalVal
read_list = do
    char '('
    x <- sepEndBy read_form ignored
    char ')'
    return $ MalList x Nil

read_vector :: Parser MalVal
read_vector = do
    char '['
    x <- sepEndBy read_form ignored
    char ']'
    return $ MalVector x Nil

-- TODO: propagate error properly
_pairs [x] = error "Odd number of elements to _pairs"
_pairs [] = []
_pairs (MalString x:y:xs) = (x,y):_pairs xs

read_hash_map :: Parser MalVal
read_hash_map = do
    char '{'
    x <- sepEndBy read_form ignored
    char '}'
    return $ MalHashMap (Map.fromList $ _pairs x) Nil

-- reader macros
read_quote :: Parser MalVal
read_quote = do
    char '\''
    x <- read_form
    return $ MalList [MalSymbol "quote", x] Nil

read_quasiquote :: Parser MalVal
read_quasiquote = do
    char '`'
    x <- read_form
    return $ MalList [MalSymbol "quasiquote", x] Nil

read_splice_unquote :: Parser MalVal
read_splice_unquote = do
    char '~'
    char '@'
    x <- read_form
    return $ MalList [MalSymbol "splice-unquote", x] Nil

read_unquote :: Parser MalVal
read_unquote = do
    char '~'
    x <- read_form
    return $ MalList [MalSymbol "unquote", x] Nil

read_deref :: Parser MalVal
read_deref = do
    char '@'
    x <- read_form
    return $ MalList [MalSymbol "deref", x] Nil

read_with_meta :: Parser MalVal
read_with_meta = do
    char '^'
    m <- read_form
    x <- read_form
    return $ MalList [MalSymbol "with-meta", x, m] Nil

read_macro :: Parser MalVal
read_macro = read_quote
         <|> read_quasiquote
         <|> try read_splice_unquote <|> read_unquote
         <|> read_deref
         <|> read_with_meta

--

read_form :: Parser MalVal
read_form =  do
    ignored
    x <- read_macro
     <|> read_list
     <|> read_vector
     <|> read_hash_map
     <|> read_atom
    return $ x

read_str :: String -> IOThrows MalVal
read_str str = case parse read_form "Mal" str of
    Left err -> throwStr $ show err
    Right val -> return val
