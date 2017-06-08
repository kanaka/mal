port module Main exposing (..)

import Array
import Dict exposing (Dict)
import IO exposing (..)
import Json.Decode exposing (decodeValue)
import Platform exposing (programWithFlags)
import Types exposing (..)
import Reader exposing (readString)
import Printer exposing (printString)
import Utils exposing (maybeToList, zip, last)
import Env
import Core
import Eval


main : Program Flags Model Msg
main =
    programWithFlags
        { init = init
        , update = update
        , subscriptions =
            \model -> input (decodeValue decodeIO >> Input)
        }


type alias Flags =
    { args : List String
    }


type alias Model =
    { args : List String
    , env : Env
    , cont : Maybe (IO -> Eval MalExpr)
    }


init : Flags -> ( Model, Cmd Msg )
init { args } =
    ( { args = args
      , env = Core.ns
      , cont = Nothing
      }
    , readLine prompt
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model.cont of
        Nothing ->
            normalUpdate msg model

        Just cont ->
            case msg of
                Input (Ok io) ->
                    run { model | cont = Nothing } (cont io)

                Input (Err msg) ->
                    Debug.crash msg ( model, Cmd.none )


normalUpdate : Msg -> Model -> ( Model, Cmd Msg )
normalUpdate msg model =
    case msg of
        Input (Ok (LineRead (Just line))) ->
            rep line
                |> Maybe.map (run model)
                |> Maybe.withDefault (( model, readLine prompt ))

        Input (Ok LineWritten) ->
            ( model, readLine prompt )

        Input (Ok (LineRead Nothing)) ->
            ( model, Cmd.none )

        Input (Err msg) ->
            Debug.crash msg ( model, Cmd.none )


run : Model -> Eval MalExpr -> ( Model, Cmd Msg )
run model e =
    case Eval.run { env = model.env } e of
        ( { env }, EvalOk expr ) ->
            ( { model | env = env }, writeLine (print expr) )

        ( { env }, EvalErr msg ) ->
            ( { model | env = env }, writeLine ("ERR:" ++ msg) )

        ( { env }, EvalIO cmd cont ) ->
            ( { model | cont = Just cont }, cmd )


makeOutput : Result String String -> String
makeOutput result =
    case result of
        Ok str ->
            str

        Err msg ->
            "ERR:" ++ msg


prompt : String
prompt =
    "user> "


{-| read can return three things:

Ok (Just expr) -> parsed okay
Ok Nothing -> empty string (only whitespace and/or comments)
Err msg -> parse error

-}
read : String -> Result String (Maybe MalExpr)
read =
    readString


eval : MalExpr -> Eval MalExpr
eval ast =
    case ast of
        MalList [] ->
            Eval.succeed ast

        MalList ((MalSymbol "def!") :: args) ->
            evalDef args

        MalList ((MalSymbol "let*") :: args) ->
            evalLet args

        MalList ((MalSymbol "do") :: args) ->
            evalDo args

        MalList ((MalSymbol "if") :: args) ->
            evalIf args

        MalList ((MalSymbol "fn*") :: args) ->
            evalFn args

        MalList list ->
            evalList list
                |> Eval.andThen
                    (\newList ->
                        case newList of
                            [] ->
                                Eval.fail "can't happen"

                            (MalFunction fn) :: args ->
                                fn args

                            fn :: _ ->
                                Eval.fail ((printString True fn) ++ " is not a function")
                    )

        _ ->
            evalAst ast


evalAst : MalExpr -> Eval MalExpr
evalAst ast =
    case ast of
        MalSymbol sym ->
            -- Lookup symbol in env and return value or raise error if not found.
            Eval.withState
                (\state ->
                    case Env.get sym state.env of
                        Ok val ->
                            Eval.succeed val

                        Err msg ->
                            Eval.fail msg
                )

        MalList list ->
            -- Return new list that is result of calling eval on each element of list.
            evalList list
                |> Eval.map MalList

        MalVector vec ->
            evalList (Array.toList vec)
                |> Eval.map (Array.fromList >> MalVector)

        MalMap map ->
            evalList (Dict.values map)
                |> Eval.map
                    (zip (Dict.keys map)
                        >> Dict.fromList
                        >> MalMap
                    )

        _ ->
            Eval.succeed ast


evalList : List MalExpr -> Eval (List MalExpr)
evalList list =
    let
        go list acc =
            case list of
                [] ->
                    Eval.succeed (List.reverse acc)

                x :: rest ->
                    eval x
                        |> Eval.andThen
                            (\val ->
                                go rest (val :: acc)
                            )
    in
        go list []


evalDef : List MalExpr -> Eval MalExpr
evalDef args =
    case args of
        [ MalSymbol name, uneValue ] ->
            eval uneValue
                |> Eval.andThen
                    (\value ->
                        Eval.modifyState
                            (\state ->
                                { state | env = Env.set name value state.env }
                            )
                            |> Eval.andThen (\_ -> Eval.succeed value)
                    )

        _ ->
            Eval.fail "def! expected two args: name and value"


evalLet : List MalExpr -> Eval MalExpr
evalLet args =
    let
        evalBinds binds =
            case binds of
                (MalSymbol name) :: expr :: rest ->
                    eval expr
                        |> Eval.andThen
                            (\value ->
                                Eval.modifyState (\state -> { state | env = Env.set name value state.env })
                                    |> Eval.andThen
                                        (\_ ->
                                            if List.isEmpty rest then
                                                Eval.succeed ()
                                            else
                                                evalBinds rest
                                        )
                            )

                _ ->
                    Eval.fail "let* expected an even number of binds (symbol expr ..)"

        go binds body =
            Eval.modifyState (\state -> { state | env = Env.make (Just state.env) })
                |> Eval.andThen (\_ -> evalBinds binds)
                |> Eval.andThen (\_ -> eval body)
    in
        case args of
            [ MalList binds, body ] ->
                go binds body

            [ MalVector bindsVec, body ] ->
                go (Array.toList bindsVec) body

            _ ->
                Eval.fail "let* expected two args: binds and a body"


evalDo : List MalExpr -> Eval MalExpr
evalDo args =
    let
        returnLast list =
            case last list of
                Just value ->
                    Eval.succeed value

                Nothing ->
                    Eval.fail "do expected at least one arg"
    in
        evalList args
            |> Eval.andThen returnLast


evalIf : List MalExpr -> Eval MalExpr
evalIf args =
    let
        isThruthy expr =
            expr /= MalNil && expr /= (MalBool False)

        go condition trueExpr falseExpr =
            eval condition
                |> Eval.map isThruthy
                |> Eval.andThen
                    (\cond ->
                        eval
                            (if cond then
                                trueExpr
                             else
                                falseExpr
                            )
                    )
    in
        case args of
            [ condition, trueExpr ] ->
                go condition trueExpr MalNil

            [ condition, trueExpr, falseExpr ] ->
                go condition trueExpr falseExpr

            _ ->
                Eval.fail "if expected at least two args"


evalFn : List MalExpr -> Eval MalExpr
evalFn args =
    let
        extractSymbols list acc =
            case list of
                [] ->
                    Ok (List.reverse acc)

                (MalSymbol name) :: rest ->
                    extractSymbols rest (name :: acc)

                _ ->
                    Err "all binds in fn* must be a symbol"

        bindArgs env pairs =
            case pairs of
                [] ->
                    env

                ( bind, arg ) :: rest ->
                    bindArgs (Env.set bind arg env) rest

        makeEnv binds args env =
            zip binds args
                |> bindArgs (Env.make (Just env))
    in
        case args of
            [ MalList bindsList, body ] ->
                case extractSymbols bindsList [] of
                    Ok binds ->
                        let
                            fn args =
                                if List.length args /= List.length binds then
                                    Eval.fail <|
                                        "function expected "
                                            ++ (toString (List.length binds))
                                            ++ " arguments, got "
                                            ++ (toString (List.length binds))
                                else
                                    -- TODO: push state and pop afterwards!
                                    -- TODO or temporary change state?
                                    Eval.withState
                                        (\state ->
                                            Eval.putState ({ state | env = makeEnv binds args state.env })
                                                |> Eval.andThen (\_ -> eval body)
                                                |> Eval.andThen (\res -> Eval.putState state |> Eval.map (\_ -> res))
                                        )
                        in
                            Eval.succeed (MalFunction fn)

                    -- TODO explicitly pass current env
                    Err msg ->
                        Eval.fail msg

            _ ->
                Eval.fail "fn* expected two args: binds list and body"


print : MalExpr -> String
print =
    printString True


{-| Read-Eval-Print.

Doesn't actually run the Eval but returns the monad.

-}
rep : String -> Maybe (Eval MalExpr)
rep input =
    case readString input of
        Ok Nothing ->
            Nothing

        Err msg ->
            Just (Eval.fail msg)

        Ok (Just ast) ->
            eval ast |> Just
