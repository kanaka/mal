module Step4_if_fn_do exposing (..)

import Array
import Core
import Dict exposing (Dict)
import Env
import Eval
import IO exposing (..)
import Json.Decode exposing (decodeValue, errorToString)
import Platform exposing (worker)
import Printer exposing (printString)
import Reader exposing (readString)
import Types exposing (..)
import Utils exposing (justValues, last, maybeToList, zip)


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

                Input (Err msg2) ->
                    Debug.todo msg2

        ReplActive env ->
            case msg of
                Input (Ok (LineRead (Just line))) ->
                    run env (rep line)

                Input (Ok LineWritten) ->
                    ( model, readLine prompt )

                Input (Ok (LineRead Nothing)) ->
                    -- Ctrl+D = The End.
                    ( model, Cmd.none )

                Input (Ok io) ->
                    Debug.todo "unexpected IO received: " io

                Input (Err msg2) ->
                    Debug.todo msg2

        ReplIO env cont ->
            case msg of
                Input (Ok io) ->
                    run env (cont io)

                Input (Err msg2) ->
                    Debug.todo msg2 ( model, Cmd.none )


runInit : Env -> Eval MalExpr -> ( Model, Cmd Msg )
runInit env0 expr0 =
    case Eval.run env0 expr0 of
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
run env0 expr0 =
    case Eval.run env0 expr0 of
        ( env, EvalOk expr ) ->
            ( ReplActive env, writeLine (print env expr) )

        ( env, EvalErr msg ) ->
            ( ReplActive env, writeLine (printError env msg) )

        ( env, EvalIO cmd cont ) ->
            ( ReplIO env cont, cmd )


prompt : String
prompt =
    "user> "


read : String -> Result String MalExpr
read =
    readString


eval : MalExpr -> Eval MalExpr
eval ast =
  Eval.withEnv (\env -> Eval.succeed <|
    case Env.get "DEBUG-EVAL" env of
        Err _              -> ()
        Ok MalNil          -> ()
        Ok (MalBool False) -> ()
        _ -> Debug.log ("EVAL: " ++ printString env True ast) ()
        --  The output ends with an ugly ": ()", but that does not hurt.
  ) |> Eval.andThen (\_ ->
    case ast of
        MalList _ [] ->
            Eval.succeed ast

        MalList _ ((MalSymbol "def!") :: args) ->
            evalDef args

        MalList _ ((MalSymbol "let*") :: args) ->
            evalLet args

        MalList _ ((MalSymbol "do") :: args) ->
            evalDo args

        MalList _ ((MalSymbol "if") :: args) ->
            evalIf args

        MalList _ ((MalSymbol "fn*") :: args) ->
            evalFn args

        MalList _ list ->
            evalList list
                |> Eval.andThen
                    (\newList ->
                        case newList of
                            [] ->
                                Eval.fail "can't happen"

                            (MalFunction (CoreFunc _ fn)) :: args ->
                                fn args

                            (MalFunction (UserFunc { eagerFn })) :: args ->
                                eagerFn args

                            fn :: _ ->
                                Eval.withEnv
                                    (\env ->
                                        Eval.fail (printString env True fn ++ " is not a function")
                                    )
                    )

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

        MalVector _ vec ->
            evalList (Array.toList vec)
                |> Eval.map (Array.fromList >> MalVector Nothing)

        MalMap _ map ->
            evalList (Dict.values map)
                |> Eval.map
                    (zip (Dict.keys map)
                        >> Dict.fromList
                        >> MalMap Nothing
                    )

        _ ->
            Eval.succeed ast
  )


evalList : List MalExpr -> Eval (List MalExpr)
evalList list =
    let
        go lst acc =
            case lst of
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
                |> Eval.andThen (\_ -> eval body)
                |> Eval.andThen
                    (\res ->
                        Eval.modifyEnv Env.pop
                            |> Eval.map (\_ -> res)
                    )
    in
    case args of
        [ MalList _ binds, body ] ->
            go binds body

        [ MalVector _ bindsVec, body ] ->
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
        isTruthy expr =
            expr /= MalNil && expr /= MalBool False

        go condition trueExpr falseExpr =
            eval condition
                |> Eval.andThen
                    (\cond ->
                        eval
                            (if isTruthy cond then
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
evalFn parms =
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
                        ++ String.fromInt numBinds
                        ++ " arguments"

            else
                Ok <| zip binds args

        bindVarArgs binds var args =
            let
                minArgs =
                    List.length binds

                varArgs =
                    MalList Nothing (List.drop minArgs args)
            in
            if List.length args < minArgs then
                Err <|
                    "function expected at least "
                        ++ String.fromInt minArgs
                        ++ " arguments"

            else
                Ok <| zip binds args ++ [ ( var, varArgs ) ]

        makeFn frameId binder body =
            MalFunction <|
                let
                    fn args =
                        case binder args of
                            Ok bound ->
                                Eval.withEnv
                                    (\env ->
                                        Eval.modifyEnv (Env.enter frameId bound)
                                            |> Eval.andThen (always (eval body))
                                            |> Eval.finally Env.leave
                                    )

                            Err msg ->
                                Eval.fail msg
                in
                UserFunc
                    { frameId = frameId
                    , lazyFn = fn
                    , eagerFn = fn
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
    case parms of
        [ MalList _ bindsList, body ] ->
            go bindsList body

        [ MalVector _ bindsVec, body ] ->
            go (Array.toList bindsVec) body

        _ ->
            Eval.fail "fn* expected two args: binds list and body"


print : Env -> MalExpr -> String
print env =
    printString env True


printError : Env -> MalExpr -> String
printError env expr =
    "Error: " ++ printString env False expr


{-| Read-Eval-Print.

Doesn't actually run the Eval but returns the monad.

-}
rep : String -> Eval MalExpr
rep input =
    case readString input of
        Err msg ->
            Eval.fail msg

        Ok ast ->
            eval ast
