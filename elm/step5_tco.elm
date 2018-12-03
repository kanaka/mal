port module Main exposing (..)

import Array
import Dict exposing (Dict)
import IO exposing (..)
import Json.Decode exposing (decodeValue)
import Platform exposing (programWithFlags)
import Types exposing (..)
import Reader exposing (readString)
import Printer exposing (printString)
import Utils exposing (maybeToList, zip, last, justValues)
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


type Model
    = InitIO Env (IO -> Eval MalExpr)
    | InitError
    | ReplActive Env
    | ReplIO Env (IO -> Eval MalExpr)


init : Flags -> ( Model, Cmd Msg )
init { args } =
    let
        initEnv =
            Core.ns

        evalMalInit =
            malInit
                |> List.map rep
                |> justValues
                |> List.foldl
                    (\b a -> a |> Eval.andThen (\_ -> b))
                    (Eval.succeed MalNil)
    in
        runInit initEnv evalMalInit


malInit : List String
malInit =
    [ """(def! not
            (fn* (a)
                (if a false true)))"""
    ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        InitError ->
            -- ignore all
            ( model, Cmd.none )

        InitIO env cont ->
            case msg of
                Input (Ok io) ->
                    runInit env (cont io)

                Input (Err msg) ->
                    Debug.crash msg

        ReplActive env ->
            case msg of
                Input (Ok (LineRead (Just line))) ->
                    case rep line of
                        Just expr ->
                            run env expr

                        Nothing ->
                            ( model, readLine prompt )

                Input (Ok LineWritten) ->
                    ( model, readLine prompt )

                Input (Ok (LineRead Nothing)) ->
                    -- Ctrl+D = The End.
                    ( model, Cmd.none )

                Input (Ok io) ->
                    Debug.crash "unexpected IO received: " io

                Input (Err msg) ->
                    Debug.crash msg

        ReplIO env cont ->
            case msg of
                Input (Ok io) ->
                    run env (cont io)

                Input (Err msg) ->
                    Debug.crash msg ( model, Cmd.none )


runInit : Env -> Eval MalExpr -> ( Model, Cmd Msg )
runInit env expr =
    case Eval.run env expr of
        ( env, EvalOk expr ) ->
            -- Init went okay, start REPL.
            ( ReplActive env, readLine prompt )

        ( env, EvalErr msg ) ->
            -- Init failed, don't start REPL.
            ( InitError, writeLine (printError env msg) )

        ( env, EvalIO cmd cont ) ->
            -- IO in init.
            ( InitIO env cont, cmd )


run : Env -> Eval MalExpr -> ( Model, Cmd Msg )
run env expr =
    case Eval.run env expr of
        ( env, EvalOk expr ) ->
            ( ReplActive env, writeLine (print env expr) )

        ( env, EvalErr msg ) ->
            ( ReplActive env, writeLine (printError env msg) )

        ( env, EvalIO cmd cont ) ->
            ( ReplIO env cont, cmd )


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


debug : String -> (Env -> a) -> Eval b -> Eval b
debug msg f e =
    Eval.withEnv
        (\env ->
            Env.debug env msg (f env)
                |> always e
        )


eval : MalExpr -> Eval MalExpr
eval ast =
    let
        apply expr env =
            case expr of
                MalApply app ->
                    Left
                        (debug "evalApply"
                            (\env -> printString env True expr)
                            (evalApply app)
                        )

                _ ->
                    Right expr
    in
        evalNoApply ast
            |> Eval.andThen (Eval.runLoop apply)


evalApply : ApplyRec -> Eval MalExpr
evalApply { frameId, bound, body } =
    Eval.withEnv
        (\env ->
            Eval.modifyEnv (Env.enter frameId bound)
                |> Eval.andThen (\_ -> evalNoApply body)
                |> Eval.finally Env.leave
                |> Eval.gcPass
        )


evalNoApply : MalExpr -> Eval MalExpr
evalNoApply ast =
    debug "evalNoApply"
        (\env -> printString env True ast)
        (case ast of
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

                                (MalFunction (CoreFunc fn)) :: args ->
                                    fn args

                                (MalFunction (UserFunc { lazyFn })) :: args ->
                                    lazyFn args

                                fn :: _ ->
                                    Eval.withEnv
                                        (\env ->
                                            Eval.fail ((printString env True fn) ++ " is not a function")
                                        )
                        )

            _ ->
                evalAst ast
        )


evalAst : MalExpr -> Eval MalExpr
evalAst ast =
    case ast of
        MalSymbol sym ->
            -- Lookup symbol in env and return value or raise error if not found.
            Eval.withEnv
                (\env ->
                    case Env.get sym env of
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
                        Eval.modifyEnv (Env.set name value)
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
                                Eval.modifyEnv (Env.set name value)
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
            Eval.modifyEnv Env.push
                |> Eval.andThen (\_ -> evalBinds binds)
                |> Eval.andThen (\_ -> evalNoApply body)
                |> Eval.andThen
                    (\res ->
                        Eval.modifyEnv Env.pop
                            |> Eval.map (\_ -> res)
                    )
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
    case List.reverse args of
        last :: rest ->
            evalList (List.reverse rest)
                |> Eval.andThen (\_ -> evalNoApply last)

        [] ->
            Eval.fail "do expected at least one arg"


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
                        evalNoApply
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
        {- Extract symbols from the binds list and verify their uniqueness -}
        extractSymbols acc list =
            case list of
                [] ->
                    Ok (List.reverse acc)

                (MalSymbol name) :: rest ->
                    if List.member name acc then
                        Err "all binds must have unique names"
                    else
                        extractSymbols (name :: acc) rest

                _ ->
                    Err "all binds in fn* must be a symbol"

        parseBinds list =
            case List.reverse list of
                var :: "&" :: rest ->
                    Ok <| bindVarArgs (List.reverse rest) var

                _ ->
                    if List.member "&" list then
                        Err "varargs separator '&' is used incorrectly"
                    else
                        Ok <| bindArgs list

        extractAndParse =
            extractSymbols [] >> Result.andThen parseBinds

        bindArgs binds args =
            let
                numBinds =
                    List.length binds
            in
                if List.length args /= numBinds then
                    Err <|
                        "function expected "
                            ++ (toString numBinds)
                            ++ " arguments"
                else
                    Ok <| zip binds args

        bindVarArgs binds var args =
            let
                minArgs =
                    List.length binds

                varArgs =
                    MalList (List.drop minArgs args)
            in
                if List.length args < minArgs then
                    Err <|
                        "function expected at least "
                            ++ (toString minArgs)
                            ++ " arguments"
                else
                    Ok <| zip binds args ++ [ ( var, varArgs ) ]

        makeFn frameId binder body =
            MalFunction <|
                let
                    lazyFn args =
                        case binder args of
                            Ok bound ->
                                Eval.succeed <|
                                    MalApply
                                        { frameId = frameId
                                        , bound = bound
                                        , body = body
                                        }

                            Err msg ->
                                Eval.fail msg
                in
                    UserFunc
                        { frameId = frameId
                        , lazyFn = lazyFn
                        , eagerFn = lazyFn >> Eval.andThen eval
                        , isMacro = False
                        , meta = Nothing
                        }

        go bindsList body =
            case extractAndParse bindsList of
                Ok binder ->
                    Eval.modifyEnv Env.ref
                        -- reference the current frame.
                        |> Eval.andThen
                            (\_ ->
                                Eval.withEnv
                                    (\env ->
                                        Eval.succeed
                                            (makeFn env.currentFrameId binder body)
                                    )
                            )

                Err msg ->
                    Eval.fail msg
    in
        case args of
            [ MalList bindsList, body ] ->
                go bindsList body

            [ MalVector bindsVec, body ] ->
                go (Array.toList bindsVec) body

            _ ->
                Eval.fail "fn* expected two args: binds list and body"


print : Env -> MalExpr -> String
print env =
    printString env True


printError : Env -> MalExpr -> String
printError env expr =
    "Error: " ++ (printString env False expr)


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
