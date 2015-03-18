%%%
%%% Core functions
%%%

-module(core).
-compile(export_all).

%%
%% Numeric functions
%%

int_op(F, [A0,A1|[]]) ->
    case A0 of
        {integer, I0} ->
            case A1 of
                {integer, I1} ->
                    F(I0, I1);
                _ -> {error, "second argument must be an integer"}
            end;
        _ -> {error, "first argument must be an integer"}
    end;
int_op(_F, _L) ->
    {error, "must have two arguments"}.

int_add(Args) ->
    {integer, int_op(fun(I, J) -> I + J end, Args)}.

int_sub(Args) ->
    {integer, int_op(fun(I, J) -> I - J end, Args)}.

int_mul(Args) ->
    {integer, int_op(fun(I, J) -> I * J end, Args)}.

int_div(Args) ->
    {integer, int_op(fun(I, J) -> I div J end, Args)}.

ns() ->
    #{
        "+" => fun int_add/1,
        "-" => fun int_sub/1,
        "*" => fun int_mul/1,
        "/" => fun int_div/1
    }.
