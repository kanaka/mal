module Reader exposing (..)

import Array
import Combine exposing (..)
import Combine.Num
import Dict
import Types exposing (MalExpr(..), keywordPrefix)
import Utils exposing (decodeString, flip, makeCall)


comment : Parser s String
comment =
    regex ";.*"


ws : Parser s (List String)
ws =
    many (whitespace |> or (string ",") |> or comment)


int : Parser s MalExpr
int =
    Combine.map MalInt Combine.Num.int
        |> onerror "int"


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
    Combine.map make symbolString
        |> onerror "symbol"


keywordString : Parser s String
keywordString =
    Combine.map (++) (string ":")
        |> andMap symbolString


keyword : Parser s MalExpr
keyword =
    Combine.map MalKeyword keywordString


list : Parser s MalExpr
list =
    Combine.map MalList (parens (many (form |> ignore ws)))
        |> onerror "list"


vector : Parser s MalExpr
vector =
    Combine.map (MalVector << Array.fromList)
        (string "["
            |> keep (many form)
            |> ignore ws
            |> ignore (string "]")
        )
        |> onerror "vector"


mapKey : Parser s String
mapKey =
    choice
        [ Combine.map (String.cons keywordPrefix) keywordString
        , Combine.map decodeString strString
        ]


mapEntry : Parser s ( String, MalExpr )
mapEntry =
    Combine.map Tuple.pair mapKey
        |> andMap form
        |> onerror "map entry"


map : Parser s MalExpr
map =
    Combine.map (MalMap << Dict.fromList)
        (string "{"
             |> keep (many (ws |> keep mapEntry))
             |> ignore ws
             |> ignore (string "}"))
        |> onerror "map"


atom : Parser s MalExpr
atom =
    choice
        [ int
        , keyword
        , symbolOrConst
        , str
        ]
        |> onerror "atom"


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
            ws |> keep (choice parsers) |> onerror "form"


simpleMacro : String -> String -> Parser s MalExpr
simpleMacro token symbol =
    string token
    |> andThen (\_ -> Combine.map (makeCall symbol << List.singleton) form)
    |> onerror symbol


withMeta : Parser s MalExpr
withMeta =
    let
        make meta expr =
            makeCall "with-meta" [ expr, meta ]
    in
        string "^"
            |> andThen (\_ -> infixAndMap (Combine.map make form) form)
            |> onerror "with-meta"


readString : String -> Result String (Maybe MalExpr)
readString stri =
    case parse (maybe form |> ignore ws |> ignore end) stri of
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
        ++ Debug.toString location.line
        ++ ":"
        ++ Debug.toString location.column
        ++ ")"


str : Parser s MalExpr
str =
    Combine.map (MalString << decodeString) strString


{-| Syntax highlighter in VS code is messed up by this regex,
that's why it's down below. :)
-}
strString : Parser s String
strString =
    regex "\"(\\\\.|[^\\\\\"])*\"" |> onerror "string"


infixAndMap : Parser s (a -> b) -> Parser s a -> Parser s b
infixAndMap =
    flip andMap
