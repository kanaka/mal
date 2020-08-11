port module Main exposing (..)

import Array
import Dict exposing (Dict)
import IO exposing (..)
import Json.Decode exposing (decodeValue)
import Platform exposing (programWithFlags)
import Types exposing (..)
import Reader exposing (readString)
import Printer exposing (printString)
import Utils exposing (maybeToList, zip, last, justValues, makeCall)
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


type alias Args =
    List String


type alias Flags =
    { args : Args
    }


type Model
    = InitIO Args Env (IO -> Eval MalExpr)
    | ScriptIO Env (IO -> Eval MalExpr)
    | ReplActive Env
    | ReplIO Env (IO -> Eval MalExpr)
    | Stopped


init : Flags -> ( Model, Cmd Msg )
init { args } =
    let
        makeFn =
            CoreFunc >> MalFunction

        initEnv =
            Core.ns
                |> Env.set "eval" (makeFn malEval)
                |> Env.set "*ARGV*" (MalList (args |> List.map MalString))

        evalMalInit =
            malInit
                |> List.map rep
                |> justValues
                |> List.foldl
                    (\b a -> a |> Eval.andThen (\_ -> b))
                    (Eval.succeed MalNil)
    in
        runInit args initEnv evalMalInit


malInit : List String
malInit =
    [ """(def! not
            (fn* (a)
                (if a false true)))"""
    , """(def! load-file
            (fn* (f)
                (eval (read-string
                    (str "(do " (slurp f) "\nnil)")))))"""
    , """(defmacro! cond
            (fn* (& xs)
                (if (> (count xs) 0)
                    (list 'if (first xs)
                        (if (> (count xs) 1)
                            (nth xs 1)
                            (throw "odd number of forms to cond"))
                        (cons 'cond (rest (rest xs)))))))"""
    ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        Stopped ->
            ( model, Cmd.none )

        InitIO args env cont ->
            case msg of
                Input (Ok io) ->
                    runInit args env (cont io)

                Input (Err msg) ->
                    Debug.crash msg

        ScriptIO env cont ->
            case msg of
                Input (Ok io) ->
                    runScriptLoop env (cont io)

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


runInit : Args -> Env -> Eval MalExpr -> ( Model, Cmd Msg )
runInit args env expr =
    case Eval.run env expr of
        ( env, EvalOk expr ) ->
            -- Init went okay.
            case args of
                -- If we got no args: start REPL.
                [] ->
                    ( ReplActive env, readLine prompt )

                -- Run the script in the first argument.
                -- Put the rest of the arguments as *ARGV*.
                filename :: argv ->
                    runScript filename argv env

        ( env, EvalErr msg ) ->
            -- Init failed, don't start REPL.
            ( Stopped, writeLine (printError env msg) )

        ( env, EvalIO cmd cont ) ->
            -- IO in init.
            ( InitIO args env cont, cmd )


runScript : String -> List String -> Env -> ( Model, Cmd Msg )
runScript filename argv env =
    let
        malArgv =
            MalList (List.map MalString argv)

        newEnv =
            env |> Env.set "*ARGV*" malArgv

        program =
            MalList
                [ MalSymbol "load-file"
                , MalString filename
                ]
    in
        runScriptLoop newEnv (eval program)


runScriptLoop : Env -> Eval MalExpr -> ( Model, Cmd Msg )
runScriptLoop env expr =
    case Eval.run env expr of
        ( env, EvalOk expr ) ->
            ( Stopped, Cmd.none )

        ( env, EvalErr msg ) ->
            ( Stopped, writeLine (printError env msg) )

        ( env, EvalIO cmd cont ) ->
            ( ScriptIO env cont, cmd )


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


malEval : List MalExpr -> Eval MalExpr
malEval args =
    case args of
        [ expr ] ->
            Eval.inGlobal (eval expr)

        _ ->
            Eval.fail "unsupported arguments"


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
    let
        go ast =
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

                MalList ((MalSymbol "quote") :: args) ->
                    evalQuote args

                MalList [MalSymbol "quasiquoteexpand", expr] ->
                    Eval.succeed <| evalQuasiQuote expr
                MalList (MalSymbol "quasiquoteexpand" :: _) ->
                    Eval.fail "quasiquoteexpand: arg count"

                MalList ((MalSymbol "quasiquote") :: args) ->
                    case args of
                        [ expr ] ->
                            -- TCO.
                            evalNoApply (evalQuasiQuote expr)

                        _ ->
                            Eval.fail "unsupported arguments"

                MalList ((MalSymbol "defmacro!") :: args) ->
                    evalDefMacro args

                MalList ((MalSymbol "macroexpand") :: args) ->
                    case args of
                        [ expr ] ->
                            macroexpand expr

                        _ ->
                            Eval.fail "unsupported arguments"

                MalList ((MalSymbol "try*") :: args) ->
                    evalTry args

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
    in
        debug "evalNoApply"
            (\env -> printString env True ast)
            (macroexpand ast |> Eval.andThen go)


evalAst : MalExpr -> Eval MalExpr
evalAst ast =
    case ast of
        MalSymbol sym ->
            -- Lookup symbol in env and return value or raise error if not found.
            Eval.withEnv (Env.get sym >> Eval.fromResult)

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


evalDefMacro : List MalExpr -> Eval MalExpr
evalDefMacro args =
    case args of
        [ MalSymbol name, uneValue ] ->
            eval uneValue
                |> Eval.andThen
                    (\value ->
                        case value of
                            MalFunction (UserFunc fn) ->
                                let
                                    macroFn =
                                        MalFunction (UserFunc { fn | isMacro = True })
                                in
                                    Eval.modifyEnv (Env.set name macroFn)
                                        |> Eval.andThen (\_ -> Eval.succeed macroFn)

                            _ ->
                                Eval.fail "defmacro! is only supported on a user function"
                    )

        _ ->
            Eval.fail "defmacro! expected two args: name and value"


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
                |> Eval.finally Env.pop
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
                    lazyFn =
                        binder
                            >> Eval.fromResult
                            >> Eval.map
                                (\bound ->
                                    MalApply
                                        { frameId = frameId
                                        , bound = bound
                                        , body = body
                                        }
                                )
                in
                    UserFunc
                        { frameId = frameId
                        , lazyFn = lazyFn
                        , eagerFn = lazyFn >> Eval.andThen eval
                        , isMacro = False
                        , meta = Nothing
                        }

        go bindsList body =
            extractAndParse bindsList
                |> Eval.fromResult
                -- reference the current frame.
                |> Eval.ignore (Eval.modifyEnv Env.ref)
                |> Eval.andThen
                    (\binder ->
                        Eval.withEnv
                            (\env ->
                                Eval.succeed
                                    (makeFn env.currentFrameId binder body)
                            )
                    )
    in
        case args of
            [ MalList bindsList, body ] ->
                go bindsList body

            [ MalVector bindsVec, body ] ->
                go (Array.toList bindsVec) body

            _ ->
                Eval.fail "fn* expected two args: binds list and body"


evalQuote : List MalExpr -> Eval MalExpr
evalQuote args =
    case args of
        [ expr ] ->
            Eval.succeed expr

        _ ->
            Eval.fail "unsupported arguments"


evalQuasiQuote : MalExpr -> MalExpr
evalQuasiQuote expr =
    let
        qq_loop : MalExpr -> MalExpr -> MalExpr
        qq_loop elt acc =
            case elt of
                (MalList [MalSymbol "splice-unquote", form]) ->
                    MalList <| [MalSymbol "concat", form, acc ]
                _ ->
                    MalList <| [MalSymbol "cons", evalQuasiQuote elt, acc ]
    in
        case expr of
            (MalList [MalSymbol "unquote", form]) ->
                    form
            (MalList xs) ->
                    List.foldr qq_loop (MalList []) xs
            (MalVector xs) ->
                    MalList <| (\x -> [MalSymbol "vec", x]) <| Array.foldr qq_loop (MalList []) xs
            (MalSymbol _) ->
                    MalList <| [MalSymbol "quote", expr]
            (MalMap _) ->
                    MalList <| [MalSymbol "quote", expr]
            _ ->
                    expr


macroexpand : MalExpr -> Eval MalExpr
macroexpand expr =
    let
        expand expr env =
            case expr of
                MalList ((MalSymbol name) :: args) ->
                    case Env.get name env of
                        Ok (MalFunction (UserFunc fn)) ->
                            if fn.isMacro then
                                Left <| fn.eagerFn args
                            else
                                Right expr

                        _ ->
                            Right expr

                _ ->
                    Right expr
    in
        Eval.runLoop expand expr


evalTry : List MalExpr -> Eval MalExpr
evalTry args =
    case args of
        [ body ] ->
            eval body
        [ body, MalList [ MalSymbol "catch*", MalSymbol sym, handler ] ] ->
            eval body
                |> Eval.catchError
                    (\ex ->
                        Eval.modifyEnv Env.push
                            |> Eval.andThen
                                (\_ ->
                                    Eval.modifyEnv (Env.set sym ex)
                                )
                            |> Eval.andThen (\_ -> eval handler)
                            |> Eval.finally Env.pop
                    )

        _ ->
            Eval.fail "try* expected a body a catch block"


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
