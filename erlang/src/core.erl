%%%
%%% Core functions
%%%

-module(core).
-compile(export_all).

count([Args]) ->
    case Args of
        {list, List} -> {integer, length(List)};
        {vector, List} -> {integer, length(List)};
        nil -> {integer, 0};
        _ -> {error, "count called on non-sequence"}
    end;
count([]) ->
    {error, "count called with no arguments"};
count(_) ->
    {error, "count expects one list argument"}.

empty_q([Args]) ->
    case Args of
        {list, List} -> length(List) == 0;
        {vector, List} -> length(List) == 0;
        _ -> {error, "empty? called on non-sequence"}
    end;
empty_q([]) ->
    {error, "empty? called with no arguments"};
empty_q(_) ->
    {error, "empty? expects one list argument"}.

equal_q(Args) ->
    case Args of
        [nil, nil] -> true;
        [true, true] -> true;
        [false, false] -> true;
        [{integer, I}, {integer, J}] -> I == J;
        [{string, S}, {string, T}] -> S == T;
        [{keyword, K}, {keyword, J}] -> K == J;
        [{symbol, S}, {symbol, T}] -> S == T;
        [{list, L1}, {list, L2}] -> L1 == L2;
        [{vector, L1}, {vector, L2}] -> L1 == L2;
        [{list, L1}, {vector, L2}] -> L1 == L2;
        [{vector, L1}, {list, L2}] -> L1 == L2;
        [{map, M1}, {map, M2}] -> M1 == M2;
        [{closure, _C1}, {closure, _C2}] -> false;
        [{function, _F1}, {function, _F2}] -> false;
        [_A, _B] -> false;
        _ -> {error, "equal? expects two arguments"}
    end.

int_op(F, [A0,A1]) ->
    case A0 of
        {integer, I0} ->
            case A1 of
                {integer, I1} ->
                    {integer, F(I0, I1)};
                _ -> {error, "second argument must be an integer"}
            end;
        _ -> {error, "first argument must be an integer"}
    end;
int_op(_F, _L) ->
    {error, "must have two arguments"}.

int_add(Args) ->
    int_op(fun(I, J) -> I + J end, Args).

int_sub(Args) ->
    int_op(fun(I, J) -> I - J end, Args).

int_mul(Args) ->
    int_op(fun(I, J) -> I * J end, Args).

int_div(Args) ->
    int_op(fun(I, J) -> I div J end, Args).

bool_op(F, [A0,A1]) ->
    case A0 of
        {integer, I0} ->
            case A1 of
                {integer, I1} ->
                    % the true or false is our return value
                    F(I0, I1);
                _ -> {error, "second argument must be an integer"}
            end;
        _ -> {error, "first argument must be an integer"}
    end;
bool_op(_F, _L) ->
    {error, "must have two arguments"}.

bool_lt(Args) ->
    bool_op(fun(I, J) -> I < J end, Args).

bool_lte(Args) ->
    bool_op(fun(I, J) -> I =< J end, Args).

bool_gt(Args) ->
    bool_op(fun(I, J) -> I > J end, Args).

bool_gte(Args) ->
    bool_op(fun(I, J) -> I >= J end, Args).

pr_str(Args) ->
    {string, printer:pr_list(Args, "", "", " ", true)}.

str(Args) ->
    {string, printer:pr_list(Args, "", "", "", false)}.

prn(Args) ->
    io:format("~s~n", [printer:pr_list(Args, "", "", " ", true)]),
    nil.

println(Args) ->
    io:format("~s~n", [printer:pr_list(Args, "", "", " ", false)]),
    nil.

ns() ->
    Builtins = #{
        "*" => fun int_mul/1,
        "+" => fun int_add/1,
        "-" => fun int_sub/1,
        "/" => fun int_div/1,
        "<" => fun bool_lt/1,
        "<=" => fun bool_lte/1,
        "=" => fun equal_q/1,
        ">" => fun bool_gt/1,
        ">=" => fun bool_gte/1,
        "count" => fun count/1,
        "empty?" => fun empty_q/1,
        "list" => fun types:list/1,
        "list?" => fun types:list_p/1,
        "pr-str" => fun pr_str/1,
        "println" => fun println/1,
        "prn" => fun prn/1,
        "str" => fun str/1
    },
    Env = env:new(undefined),
    SetEnv = fun(K, V) ->
        env:set(Env, {symbol, K}, types:func(V))
    end,
    maps:map(SetEnv, Builtins),
    Env.
