module Step3_env exposing (..)

import Array
import Dict exposing (Dict)
import Env
import Eval
import IO exposing (..)
import Json.Decode exposing (decodeValue, errorToString)
import Platform exposing (worker)
import Printer exposing (printString)
import Reader exposing (readString)
import Tuple exposing (mapFirst, mapSecond, second)
import Types exposing (..)
import Utils exposing (maybeToList, zip)


main : Program Flags Model Msg
main =
    worker
        { init = init
        , update = update
        , subscriptions =
            \model -> input (decodeValue decodeIO >> (\x -> case x of
                Err e -> Err (errorToString e)
                Ok a  -> Ok a
            ) >>  Input)
        }


type alias Flags =
    { args : List String
    }


type alias Model =
    { args : List String
    , env : Env
    }


type Msg
    = Input (Result String IO)


init : Flags -> ( Model, Cmd Msg )
init { args } =
    ( { args = args, env = initReplEnv }, readLine prompt )


initReplEnv : Env
initReplEnv =
    let
        makeFn =
            CoreFunc Nothing >> MalFunction

        binaryOp fn args =
            case args of
                [ MalInt x, MalInt y ] ->
                    Eval.succeed <| MalInt (fn x y)

                _ ->
                    Eval.fail "unsupported arguments"
    in
    Env.global
        |> Env.set "+" (makeFn <| binaryOp (+))
        |> Env.set "-" (makeFn <| binaryOp (-))
        |> Env.set "*" (makeFn <| binaryOp (*))
        |> Env.set "/" (makeFn <| binaryOp (//))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Input (Ok (LineRead (Just line))) ->
            let ( result, newEnv) = rep model.env line
            in ( { model | env = newEnv }, writeLine (makeOutput result) )

        Input (Ok LineWritten) ->
            ( model, readLine prompt )

        Input (Ok (LineRead Nothing)) ->
            ( model, Cmd.none )

        Input (Ok io) ->
            Debug.todo "unexpected IO received: " io

        Input (Err msg2) ->
            Debug.todo msg2 ( model, Cmd.none )


makeOutput : Result String String -> String
makeOutput result =
    case result of
        Ok str ->
            str

        Err msg ->
            "Error: " ++ msg


prompt : String
prompt =
    "user> "


read : String -> Result String MalExpr
read =
    readString


eval : Env -> MalExpr -> ( Result String MalExpr, Env )
eval env ast =
  let
    _ = case Env.get "DEBUG-EVAL" env of
        Err _              -> ()
        Ok MalNil          -> ()
        Ok (MalBool False) -> ()
        _ -> Debug.log ("EVAL: " ++ printString env True ast) ()
        --  The output ends with an ugly ": ()", but that does not hurt.
  in
  case ast of
        MalList _ [] ->
            ( Ok ast, env )

        MalList _ ((MalSymbol "def!") :: args) ->
            evalDef env args

        MalList _ ((MalSymbol "let*") :: args) ->
            evalLet env args

        MalList _ list ->
            case evalList env list [] of
                ( Ok newList, newEnv ) ->
                    case newList of
                        [] ->
                            ( Err "can't happen", newEnv )

                        (MalFunction (CoreFunc _ fn)) :: args ->
                            case Eval.runSimple (fn args) of
                                Ok res ->
                                    ( Ok res, newEnv )

                                Err msg ->
                                    ( Err (print msg), newEnv )

                        fn :: _ ->
                            ( Err ((print fn) ++ " is not a function"), newEnv )

                ( Err msg, newEnv ) ->
                    ( Err msg, newEnv )

        MalSymbol sym ->
            -- Lookup symbol in env and return value or raise error if not found.
            case Env.get sym env of
                Ok val ->
                    ( Ok val, env )

                Err msg ->
                    ( Err msg, env )

        MalVector _ vec ->
            evalList env (Array.toList vec) []
                |> mapFirst (Result.map (Array.fromList >> MalVector Nothing))

        MalMap _ map ->
            evalList env (Dict.values map) []
                |> mapFirst
                    (Result.map
                        (zip (Dict.keys map)
                            >> Dict.fromList
                            >> MalMap Nothing
                        )
                    )

        _ ->
            ( Ok ast, env )


evalList : Env -> List MalExpr -> List MalExpr -> ( Result String (List MalExpr), Env )
evalList env list acc =
    case list of
        [] ->
            ( Ok (List.reverse acc), env )

        x :: rest ->
            case eval env x of
                ( Ok val, newEnv ) ->
                    evalList newEnv rest (val :: acc)

                ( Err msg, newEnv ) ->
                    ( Err msg, newEnv )


evalDef : Env -> List MalExpr -> ( Result String MalExpr, Env )
evalDef env args =
    case args of
        [ MalSymbol name, uneValue ] ->
            case eval env uneValue of
                ( Ok value, newEnv ) ->
                    ( Ok value, Env.set name value newEnv )

                err ->
                    err

        _ ->
            ( Err "def! expected two args: name and value", env )


evalLet : Env -> List MalExpr -> ( Result String MalExpr, Env )
evalLet env args =
    let
        evalBinds env2 binds =
            case binds of
                (MalSymbol name) :: expr :: rest ->
                    case eval env2 expr of
                        ( Ok value, newEnv ) ->
                            let
                                newEnv2 =
                                    Env.set name value env2
                            in
                            if List.isEmpty rest then
                                Ok newEnv2
                            else
                                evalBinds newEnv2 rest

                        ( Err msg, _ ) ->
                            Err msg

                _ ->
                    Err "let* expected an even number of binds (symbol expr ..)"

        go binds body =
            case evalBinds (Env.push env) binds of
                Ok newEnv ->
                    eval newEnv body
                        |> mapSecond (\_ -> Env.pop newEnv)

                Err msg ->
                    ( Err msg, env )
    in
    case args of
        [ MalList _ binds, body ] ->
            go binds body

        [ MalVector _ bindsVec, body ] ->
            go (Array.toList bindsVec) body

        _ ->
            ( Err "let* expected two args: binds and a body", env )


{-| Try to map a list with a fn that can return a Err.

Maps the list from left to right. As soon as a error
occurs it will not process any more elements and return
the error.

-}
tryMapList : (a -> Result e b) -> List a -> Result e (List b)
tryMapList fn list =
    let
        go x =
            Result.andThen
                (\acc ->
                    case fn x of
                        Ok val ->
                            Ok (val :: acc)

                        Err msg ->
                            Err msg
                )
    in
    List.foldl go (Ok []) list
        |> Result.map List.reverse


print : MalExpr -> String
print =
    printString Env.global True


{-| Read-Eval-Print
-}
rep : Env -> String -> ( Result String String, Env )
rep env input =
    let
        evalPrint =
            eval env >> mapFirst (Result.map print)
    in
    case readString input of
        Err msg ->
            ( Err msg, env )

        Ok ast ->
            evalPrint ast
