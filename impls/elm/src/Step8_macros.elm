module Step8_macros exposing (..)

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
import Utils exposing (justValues, last, makeCall, maybeToList, zip)


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
            CoreFunc Nothing >> MalFunction

        initEnv =
            Core.ns
                |> Env.set "eval" (makeFn malEval)
                |> Env.set "*ARGV*" (MalList Nothing (args |> List.map MalString))

        evalMalInit =
            malInit
                |> List.map rep
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

                Input (Err msg2) ->
                    Debug.todo msg2

        ScriptIO env cont ->
            case msg of
                Input (Ok io) ->
                    runScriptLoop env (cont io)

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


runInit : Args -> Env -> Eval MalExpr -> ( Model, Cmd Msg )
runInit args env0 expr0 =
    case Eval.run env0 expr0 of
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
            MalList Nothing (List.map MalString argv)

        newEnv =
            env |> Env.set "*ARGV*" malArgv

        program =
            MalList Nothing
                [ MalSymbol "load-file"
                , MalString filename
                ]
    in
    runScriptLoop newEnv (eval program)


runScriptLoop : Env -> Eval MalExpr -> ( Model, Cmd Msg )
runScriptLoop env0 expr0 =
    case Eval.run env0 expr0 of
        ( env, EvalOk expr ) ->
            ( Stopped, Cmd.none )

        ( env, EvalErr msg ) ->
            ( Stopped, writeLine (printError env msg) )

        ( env, EvalIO cmd cont ) ->
            ( ScriptIO env cont, cmd )


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
                            (\env2 -> printString env2 True expr)
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
  Eval.withEnv (\env -> Eval.succeed <|
    case Env.get "DEBUG-EVAL" env of
        Err _              -> ()
        Ok MalNil          -> ()
        Ok (MalBool False) -> ()
        _ -> Debug.log ("EVAL: " ++ printString env True ast) ()
        --  The output ends with an ugly ": ()", but that does not hurt.
  ) |> Eval.andThen (\_ ->
    case ast of
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

        MalList _ ((MalSymbol "quote") :: args) ->
            evalQuote args

        MalList _ ((MalSymbol "quasiquote") :: args) ->
            case args of
                [ expr ] ->
                    -- TCO.
                    evalNoApply (evalQuasiQuote expr)

                _ ->
                    Eval.fail "unsupported arguments"

        MalList _ ((MalSymbol "defmacro!") :: args) ->
            evalDefMacro args

        MalList _ (a0 :: rest) ->
            eval a0
                |> Eval.andThen
                    (\f ->
                        case f of
                            MalFunction (CoreFunc _ fn) ->
                                let args = evalList rest in Eval.andThen
                                fn args

                            MalFunction (UserFunc {isMacro, eagerFn, lazyFn}) ->
                              if isMacro then
                                Eval.andThen evalNoApply (eagerFn rest)

                              else
                                let args = evalList rest in Eval.andThen
                                lazyFn args

                            fn ->
                                Eval.withEnv
                                    (\env ->
                                        Eval.fail (printString env True fn ++ " is not a function")
                                    )
                    )

        MalSymbol sym ->
            -- Lookup symbol in env and return value or raise error if not found.
            Eval.withEnv (Env.get sym >> Eval.fromResult)

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
        [ MalList _ binds, body ] ->
            go binds body

        [ MalVector _ bindsVec, body ] ->
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
        isTruthy expr =
            expr /= MalNil && expr /= MalBool False

        go condition trueExpr falseExpr =
            eval condition
                |> Eval.andThen
                    (\cond ->
                        evalNoApply
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
    case parms of
        [ MalList _ bindsList, body ] ->
            go bindsList body

        [ MalVector _ bindsVec, body ] ->
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
                (MalList _ [MalSymbol "splice-unquote", form]) ->
                    makeCall "concat" [ form, acc ]
                _ ->
                    makeCall "cons" [ evalQuasiQuote elt, acc ]
    in
    case expr of
        MalList _ [MalSymbol "unquote", form] ->
                form

        MalList _ xs ->
                List.foldr qq_loop (MalList Nothing []) xs

        MalVector _ xs ->
                makeCall "vec" [ Array.foldr qq_loop (MalList Nothing []) xs ]

        MalSymbol _ ->
                makeCall "quote" [ expr ]

        MalMap _ _ ->
                makeCall "quote" [ expr ]

        _ ->
                expr


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
