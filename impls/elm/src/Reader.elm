module Reader exposing (..)

import Array
import Dict
import Parser exposing (DeadEnd, Parser, lazy, (|.), (|=))
import Types exposing (MalExpr(..), keywordPrefix)
import Utils exposing (decodeString, makeCall)


comment : Parser ()
comment =
    Parser.lineComment ";"


ws : Parser ()
ws =
    let
        isSpaceChar : Char -> Bool
        isSpaceChar c = List.member c [' ', '\n', '\r', ',']
    in
    Parser.succeed ()
        |. Parser.sequence
            { start     = ""
            , separator = ""
            , end       = ""
            , spaces    = Parser.chompWhile isSpaceChar
            , item      = comment
            , trailing  = Parser.Optional
            }


int : Parser MalExpr
int =
    --  Parser.map MalInt Parser.int fails with elm/parser 1.1.0
    let
        isDigit : Char -> Bool
        isDigit c = '0' <= c && c <= '9'
        toInt s = case String.toInt s of
            Just r  -> MalInt r
            Nothing -> Debug.todo "should not happen"
    in
    Parser.map toInt <| Parser.getChompedString <|
        Parser.chompIf isDigit
        |. Parser.chompWhile isDigit


symbolString : Parser String
symbolString =
    let
        isSymbolChar : Char -> Bool
        isSymbolChar c =
            not (List.member c [' ', '\n', '\r', ',', '\\', '[', ']',
                '{', '}', '(', '\'', '"', '`', ';', ')'])
    in
    Parser.getChompedString <|
        Parser.chompIf isSymbolChar
        |. Parser.chompWhile isSymbolChar


symbolOrConst : Parser MalExpr
symbolOrConst =
    let
        make sym =
            case sym of
                "nil" ->
                    MalNil

                "true" ->
                    MalBool True

                "false" ->
                    MalBool False

                _ ->
                    MalSymbol sym
    in
    Parser.map make symbolString


keywordString : Parser String
keywordString =
    Parser.succeed identity
        |. Parser.token ":"
        |= symbolString


keyword : Parser MalExpr
keyword =
    Parser.map MalKeyword keywordString


list : Parser MalExpr
list =
    Parser.map (MalList Nothing) <| Parser.sequence
        { start     = "("
        , separator = ""
        , end       = ")"
        , spaces    = ws
        , item      = form
        , trailing  = Parser.Optional
        }


vector : Parser MalExpr
vector =
    Parser.map (MalVector Nothing << Array.fromList) <| Parser.sequence
        { start     = "["
        , separator = ""
        , end       = "]"
        , spaces    = ws
        , item      = form
        , trailing  = Parser.Optional
        }


mapKey : Parser String
mapKey =
    Parser.oneOf
        [ Parser.map (String.cons keywordPrefix) keywordString
        , Parser.map decodeString strString
        ]


mapEntry : Parser ( String, MalExpr )
mapEntry =
    Parser.succeed Tuple.pair |= mapKey |= form


map : Parser MalExpr
map =
    Parser.map (MalMap Nothing << Dict.fromList) <| Parser.sequence
        { start     = "{"
        , separator = ""
        , end       = "}"
        , spaces    = ws
        , item      = mapEntry
        , trailing  = Parser.Optional
      }


atom : Parser MalExpr
atom =
    Parser.oneOf
        [ Parser.succeed identity
          |. Parser.token "-"
          |= Parser.oneOf
              [ Parser.map (MalInt << negate) Parser.int
              , Parser.map (MalSymbol << (++) "-") symbolString
              , Parser.succeed (MalSymbol "-")
              ]
        , int
        , keyword
        , symbolOrConst
        , str
        ]


form : Parser MalExpr
form =
    lazy <|
        \() ->
            let
                parsers =
                    [ list
                    , vector
                    , map
                    , simpleMacro "'" "quote"
                    , simpleMacro "`" "quasiquote"
                    , simpleMacro "~@" "splice-unquote"
                    , simpleMacro "~" "unquote"
                    , simpleMacro "@" "deref"
                    , withMeta
                    , atom
                    ]
            in
            Parser.succeed identity |. ws |= Parser.oneOf parsers


simpleMacro : String -> String -> Parser MalExpr
simpleMacro token symbol =
  Parser.succeed (makeCall symbol << List.singleton)
      |. Parser.token token
      |= form


withMeta : Parser MalExpr
withMeta =
            let
                make meta expr =
                    makeCall "with-meta" [ expr, meta ]
            in
            Parser.succeed make
                |. Parser.token "^"
                |= form
                |= form


readString : String -> Result String MalExpr
readString str2 =
    case Parser.run (form |. ws |. Parser.end) str2 of
        Ok ast ->
            Ok ast

        Err deadEnds ->
            --  Should become Err <| Parser.deadEndsToString deadEnds
            --  once the function is implemented.
            Err <| formatError deadEnds


formatError : List DeadEnd -> String
formatError =
    let
        format1 deadEnd =
            Debug.toString deadEnd.problem
            ++ " at "
            ++ String.fromInt deadEnd.row
            ++ ":"
            ++ String.fromInt deadEnd.col
    in
    (++) "end of input\n" << String.join "\n" << List.map format1


str : Parser MalExpr
str =
    Parser.map (MalString << decodeString) strString


strString : Parser String
strString =
    let
        isStringNormalChar : Char -> Bool
        isStringNormalChar c = not <| List.member c ['"', '\\']
    in
    Parser.getChompedString <|
        Parser.sequence
            { start     = "\""
            , separator = ""
            , end       = "\""
            , spaces    = Parser.succeed ()
            , item      = Parser.oneOf
                [ Parser.chompIf isStringNormalChar
                    |. Parser.chompWhile isStringNormalChar
                , Parser.token "\\"
                    |. Parser.chompIf (\_ -> True)
                ]
            , trailing  = Parser.Forbidden
            }
