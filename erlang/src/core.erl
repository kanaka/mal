%%%
%%% Core functions
%%%

-module(core).
-compile(export_all).

count([{Type, List}]) when Type == list orelse Type == vector ->
    {integer, length(List)};
count([nil]) ->
    {integer, 0};
count([_]) ->
    {error, "count called on non-sequence"};
count([]) ->
    {error, "count called with no arguments"};
count(_) ->
    {error, "count expects one list argument"}.

empty_q([{Type, List}]) when Type == list orelse Type == vector ->
    length(List) == 0;
empty_q([_]) ->
    {error, "empty? called on non-sequence"};
empty_q([]) ->
    {error, "empty? called with no arguments"};
empty_q(_) ->
    {error, "empty? expects one list argument"}.

nth([{Type, List}, {integer, Index}]) when Type == list orelse Type == vector ->
    try lists:nth(Index+1, List) of
        Result -> Result
    catch
        error:_Error -> {error, "nth: index out of range"}
    end;
nth([_]) ->
    {error, "nth expects two arguments"}.

first([{Type, [First|_Rest]}]) when Type == list orelse Type == vector ->
    First;
first([{Type, []}]) when Type == list orelse Type == vector ->
    nil;
first([nil]) ->
    nil;
first([_]) ->
    {error, "first called on non-sequence"};
first([]) ->
    {error, "first called with no arguments"};
first(_) ->
    {error, "first expects one list argument"}.

rest([{Type, [_First|Rest]}]) when Type == list orelse Type == vector ->
    {list, Rest};
rest([{Type, []}]) when Type == list orelse Type == vector ->
    {list, []};
rest([_]) ->
    {error, "rest called on non-sequence"};
rest([]) ->
    {error, "rest called with no arguments"};
rest(_) ->
    {error, "rest expects one list argument"}.

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

read_string([{string, Input}]) ->
    case reader:read_str(Input) of
        {ok, AST} -> AST;
        {error, Reason} -> {error, Reason}
    end;
read_string(_) ->
    {error, "read-string requires a single string argument"}.

slurp([{string, Filepath}]) ->
    case file:read_file(Filepath) of
        {ok, Binary} -> {string, binary_to_list(Binary)};
        {error, Reason} -> {error, Reason}
    end;
slurp(_) ->
    {error, "slurp called with non-string"}.

cons([Elem, {Type, List}]) when Type == list orelse Type == vector ->
    {list, [Elem|List]};
cons([_,_]) ->
    {error, "second argument to cons must be a sequence"};
cons(_) ->
    {error, "cons expects two arguments"}.

concat(Args) ->
    PushAll = fun(Elem, AccIn) ->
        case Elem of
            {Type, List} when Type == list orelse Type == vector ->
                AccIn ++ List;
            _ -> throw("concat called with non-sequence")
        end
    end,
    try lists:foldl(PushAll, [], Args) of
        Result -> {list, Result}
    catch
        throw:Reason -> {error, Reason}
    end.

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
        "concat" => fun concat/1,
        "cons" => fun cons/1,
        "count" => fun count/1,
        "empty?" => fun empty_q/1,
        "first" => fun first/1,
        "list" => fun types:list/1,
        "list?" => fun types:list_p/1,
        "nth" => fun nth/1,
        "pr-str" => fun pr_str/1,
        "println" => fun println/1,
        "prn" => fun prn/1,
        "read-string" => fun read_string/1,
        "rest" => fun rest/1,
        "slurp" => fun slurp/1,
        "str" => fun str/1
    },
    Env = env:new(undefined),
    SetEnv = fun(K, V) ->
        env:set(Env, {symbol, K}, types:func(V))
    end,
    maps:map(SetEnv, Builtins),
    Env.
