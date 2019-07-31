module Reader exposing (..)

import Array
import Dict
import Combine exposing (..)
import Combine.Num
import Types exposing (MalExpr(..), keywordPrefix)
import Utils exposing (decodeString, makeCall)


comment : Parser s String
comment =
    regex ";.*"


ws : Parser s (List String)
ws =
    many (comment <|> string "," <|> whitespace)


int : Parser s MalExpr
int =
    MalInt <$> Combine.Num.int <?> "int"


symbolString : Parser s String
symbolString =
    regex "[^\\s\\[\\]{}('\"`,;)]+"


symbolOrConst : Parser s MalExpr
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
        make
            <$> symbolString
            <?> "symbol"


keywordString : Parser s String
keywordString =
    (++)
        <$> string ":"
        <*> symbolString


keyword : Parser s MalExpr
keyword =
    MalKeyword <$> keywordString


list : Parser s MalExpr
list =
    MalList
        <$> parens (many form <* ws)
        <?> "list"


vector : Parser s MalExpr
vector =
    MalVector
        << Array.fromList
        <$> (string "["
                *> many form
                <* ws
                <* string "]"
            )
        <?> "vector"


mapKey : Parser s String
mapKey =
    choice
        [ String.cons keywordPrefix <$> keywordString
        , decodeString <$> strString
        ]


mapEntry : Parser s ( String, MalExpr )
mapEntry =
    (,) <$> mapKey <*> form <?> "map entry"


map : Parser s MalExpr
map =
    lazy <|
        \() ->
            MalMap
                << Dict.fromList
                <$> (string "{"
                        *> many (ws *> mapEntry)
                        <* ws
                        <* string "}"
                    )
                <?> "map"


atom : Parser s MalExpr
atom =
    choice
        [ int
        , keyword
        , symbolOrConst
        , str
        ]
        <?> "atom"


form : Parser s MalExpr
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
                ws *> choice parsers <?> "form"


simpleMacro : String -> String -> Parser s MalExpr
simpleMacro token symbol =
    makeCall symbol
        << List.singleton
        <$> (string token *> form)
        <?> symbol


withMeta : Parser s MalExpr
withMeta =
    lazy <|
        \() ->
            let
                make meta expr =
                    makeCall "with-meta" [ expr, meta ]
            in
                make
                    <$> (string "^" *> form)
                    <*> form
                    <?> "with-meta"


readString : String -> Result String (Maybe MalExpr)
readString str =
    case parse ((maybe form) <* ws <* end) str of
        Ok ( _, _, ast ) ->
            Ok ast

        Err ( _, stream, ms ) ->
            Err <| formatError ms stream


formatError : List String -> InputStream -> String
formatError ms stream =
    let
        location =
            currentLocation stream
    in
        "Parse error: "
            ++ String.join ", " ms
            ++ " "
            ++ "(at "
            ++ toString location.line
            ++ ":"
            ++ toString location.column
            ++ ")"


str : Parser s MalExpr
str =
    MalString << decodeString <$> strString


{-| Syntax highlighter in VS code is messed up by this regex,
that's why it's down below. :)
-}
strString : Parser s String
strString =
    regex "\"(\\\\.|[^\\\\\"])*\"" <?> "string"
