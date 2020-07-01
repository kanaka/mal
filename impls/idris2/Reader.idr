module Reader

import Types

import Control.Monad.Syntax
import Data.List
import Data.Strings
import Text.Lexer
import Text.Parser
import Text.Token

data TokenKind
  = TSymbol
  | TKeyword
  | TNumber
  | TString
  | TOpenParen | TCloseParen
  | TOpenBrace | TCloseBrace
  | TOpenBracket | TCloseBracket
  | TSigil
  | TWhitespace

implementation Eq TokenKind where
  TSymbol == TSymbol = True
  TKeyword == TKeyword = True
  TNumber == TNumber = True
  TString == TString = True
  TOpenParen == TOpenParen = True
  TCloseParen == TCloseParen = True
  TOpenBrace == TOpenBrace = True
  TCloseBrace == TCloseBrace = True
  TOpenBracket == TOpenBracket = True
  TCloseBracket == TCloseBracket = True
  TWhitespace == TWhitespace = True
  TSigil == TSigil = True
  _ == _ = False

unescape : List Char -> List Char
unescape ('\\'::'\\'::rest) = '\\' :: unescape rest
unescape ('\\'::'"'::rest) = '"' :: unescape rest
unescape ('\\'::'n'::rest) = '\n' :: unescape rest
unescape (c::rest) = c :: unescape rest
unescape [] = []

implementation Text.Token.TokenKind TokenKind where
  TokType TSymbol = String
  TokType TKeyword = String
  TokType TNumber = Integer
  TokType TString = String
  TokType TSigil = String
  TokType _ = ()

  tokValue TSymbol x = x
  tokValue TKeyword x = assert_total $ strTail x -- keywords always start with something
  tokValue TNumber x = cast x
  tokValue TString x = pack $ unescape $ unpack $ substr 1 (length x `minus` 2) x
  tokValue TSigil x = x
  tokValue TOpenParen _ = ()
  tokValue TCloseParen _ = ()
  tokValue TOpenBrace _ = ()
  tokValue TCloseBrace _ = ()
  tokValue TOpenBracket _ = ()
  tokValue TCloseBracket _ = ()
  tokValue TWhitespace _ = ()

Token : Type
Token = Text.Token.Token TokenKind
-- Token = TokenData (Text.Token.Token TokenKind)

%hide Text.Token.TokenKind
%hide Text.Token.Token

isIdentChar : Char -> Bool
isIdentChar x = not $ isSpace x || (x `elem` unpack "[]{}()'\"`,;")

tokenMap : TokenMap Token
tokenMap = toTokenMap [
  (some (space <|> is ','), TWhitespace),
  (exact "~@", TSigil),
  (oneOf "'`~^@", TSigil),
  (is '(', TOpenParen), (is ')', TCloseParen),
  (is '{', TOpenBrace), (is '}', TCloseBrace),
  (is '[', TOpenBracket), (is ']', TCloseBracket),
  (stringLit, TString),
  (lineComment (is ';'), TWhitespace),
  (intLit, TNumber),
  (is ':' <+> some (pred isIdentChar), TKeyword),
  (some (pred isIdentChar), TSymbol)
  ]

lexText : String -> Either String (List Token)
lexText text =
  case lex tokenMap text of
       (toks, (_, _, "")) => Right $ map tok toks
       (_, (row, col, _)) => Left $
         "Lexer error at " ++ show row ++ ":" ++ show col ++ ": possibly unbalanced string"

Parser : Type -> Type
Parser = Grammar Token True

mutual
  stringGrammar : Parser String
  stringGrammar = match TString <|> map (strCons '\xff') (match TKeyword)

  listGrammar : Parser AST
  listGrammar = match TOpenParen *> map (List False) (many grammar) <* match TCloseParen

  vectorGrammar : Parser AST
  vectorGrammar = match TOpenBracket *> map (List True) (many grammar) <* match TCloseBracket

  mapGrammar : Parser AST
  mapGrammar = match TOpenBrace *> map (Map . fromList) contents <* match TCloseBrace
    where contents : Grammar Token False (List (String, AST))
          contents = many [| MkPair stringGrammar grammar |]

  sigilGrammar : Parser AST
  sigilGrammar = assert_total $ match TSigil >>= sigilAction
    where partial
          sigilAction : String -> Parser AST
          sigilAction "~@" = map (\x => List False [Symbol "splice-unquote", x]) grammar
          sigilAction "'" = map (\x => List False [Symbol "quote", x]) grammar
          sigilAction "`" = map (\x => List False [Symbol "quasiquote", x]) grammar
          sigilAction "~" = map (\x => List False [Symbol "unquote", x]) grammar
          sigilAction "@" = map (\x => List False [Symbol "deref", x]) grammar
          sigilAction "^" = flip WithMeta <$> grammar <*> grammar

  grammar : Parser AST
  grammar = choice [
    map Symbol $ match TSymbol,
    map Str stringGrammar,
    map Number $ match TNumber,
    listGrammar,
    vectorGrammar,
    mapGrammar,
    sigilGrammar
    ]

parseTokens : List Token -> Either String (List AST)
parseTokens toks =
  case parse (many grammar) toks of
       Right (ast, []) => Right ast
       Right (ast, rest) => Left $ "Parse error: possible unbalanced parentheses"
       Left (Error msg _) => Left msg

export
parseText : String -> Either String (List AST)
parseText = parseTokens . filter notWhitespace <=< lexText
  where notWhitespace : Token -> Bool
        notWhitespace t =
          case kind t of
               TWhitespace => False
               _ => True
