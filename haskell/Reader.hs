module Reader
( read_str )
where

import Text.ParserCombinators.Parsec (
    Parser, parse, space, char, digit, letter,
    (<|>), oneOf, noneOf, many, many1, skipMany1, sepEndBy)
import qualified Data.Map as Map
import Control.Monad (liftM)

import Types

spaces :: Parser ()
spaces = skipMany1 (oneOf ", ")

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
read_number = liftM (MalNumber . read) $ many1 digit

read_string :: Parser MalVal
read_string = do
    char '"'
--    x <- stringChars
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
    return $ MalKeyword x

read_atom :: Parser MalVal
read_atom =  read_number
         <|> read_string
         <|> read_keyword
         <|> read_symbol

read_list :: Parser MalVal
read_list = do
    char '('
    x <- sepEndBy read_form spaces
    char ')'
    return $ MalList x

read_vector :: Parser MalVal
read_vector = do
    char '['
    x <- sepEndBy read_form spaces
    char ']'
    return $ MalVector x

-- TODO: propagate error properly
_pairs [x] = error "Odd number of element for hashmap"
_pairs [] = []
_pairs (MalString x:y:xs) = (x,y):_pairs xs
_pairs (MalKeyword x:y:xs) = ("\x029e" ++ x,y):_pairs xs

read_hash_map :: Parser MalVal
read_hash_map = do
    char '{'
    x <- sepEndBy read_form spaces
    char '}'
    return $ MalHashMap $ Map.fromList $ _pairs x


read_form :: Parser MalVal
read_form =  do
    many spaces
    x <- read_atom <|> read_list <|> read_vector <|> read_hash_map
    return $ x

read_str :: String -> IO MalVal
read_str str = case parse read_form "Mal" str of
    Left err -> error $ "Blah: " ++ (show err)
    Right val -> return val
