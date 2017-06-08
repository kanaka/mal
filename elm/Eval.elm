module Eval exposing (..)

import Types exposing (..)
import IO exposing (IO)


apply : Eval a -> EvalState -> EvalContext a
apply (Eval f) state =
    f state


run : EvalState -> Eval a -> EvalContext a
run state e =
    apply e state


withState : (EvalState -> Eval a) -> Eval a
withState f =
    Eval <|
        \state ->
            apply (f state) state


putState : EvalState -> Eval ()
putState state =
    Eval <|
        \_ ->
            apply (succeed ()) state


modifyState : (EvalState -> EvalState) -> Eval ()
modifyState f =
    Eval <|
        \state ->
            apply (succeed ()) (f state)


succeed : a -> Eval a
succeed res =
    Eval <|
        \state ->
            ( state, EvalOk res )


io : Cmd Msg -> (IO -> Eval a) -> Eval a
io cmd cont =
    Eval <|
        \state ->
            ( state, EvalIO cmd cont )


map : (a -> b) -> Eval a -> Eval b
map f e =
    Eval <|
        \state ->
            case apply e state of
                ( state, EvalOk res ) ->
                    ( state, EvalOk (f res) )

                ( state, EvalErr msg ) ->
                    ( state, EvalErr msg )

                ( state, EvalIO cmd cont ) ->
                    ( state, EvalIO cmd (cont >> map f) )


andThen : (a -> Eval b) -> Eval a -> Eval b
andThen f e =
    Eval <|
        \state ->
            case apply e state of
                ( state, EvalOk res ) ->
                    apply (f res) state

                ( state, EvalErr msg ) ->
                    ( state, EvalErr msg )

                ( state, EvalIO cmd cont ) ->
                    ( state, EvalIO cmd (cont >> andThen f) )



-- Debug.log "wrapping EvalIO" ( state, EvalIO cmd cont )


fail : String -> Eval a
fail msg =
    Eval <|
        \state ->
            ( state, EvalErr msg )
