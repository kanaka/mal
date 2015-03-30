%%%
%%% Core functions
%%%

-module(core).
-compile(export_all).

nil_p([Arg]) ->
    Arg == nil;
nil_p(_) ->
    {error, "nil? takes a single argument"}.

true_p([Arg]) ->
    Arg == true;
true_p(_) ->
    {error, "true? takes a single argument"}.

false_p([Arg]) ->
    Arg == false;
false_p(_) ->
    {error, "false? takes a single argument"}.

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
            _ -> error("concat called with non-sequence")
        end
    end,
    try lists:foldl(PushAll, [], Args) of
        Result -> {list, Result}
    catch
        error:Reason -> {error, Reason}
    end.

mal_throw([Reason]) ->
    throw(Reason);
mal_throw(_) ->
    {error, "throw expects a list with one argument"}.

map_f([{closure, Eval, Binds, Body, CE}, {Type, Args}]) when Type == list orelse Type == vector ->
    Apply = fun(Arg) ->
        NewEnv = env:new(CE),
        env:bind(NewEnv, Binds, [Arg]),
        Eval(Body, NewEnv)
    end,
    {list, lists:map(Apply, Args)};
map_f([{function, F}, {Type, Args}]) when Type == list orelse Type == vector ->
    {list, [erlang:apply(F, [[Arg]]) || Arg <- Args]};
map_f(_) ->
    {error, "map expects a function and list argument"}.

process_args(Args) ->
    % Convert the apply arguments into a flat list, such that no element
    % consists of {list,...} or {vector,...} (i.e. just [A, B, C, ...]).
    Delist = fun(Elem) ->
        case Elem of
            {T, L} when T == list orelse T == vector -> L;
            _ -> Elem
        end
    end,
    lists:flatten(lists:map(Delist, lists:flatten(Args))).

apply_f([{closure, Eval, Binds, Body, CE}|Args]) ->
    NewEnv = env:new(CE),
    env:bind(NewEnv, Binds, process_args(Args)),
    Eval(Body, NewEnv);
apply_f([{function, F}|Args]) ->
    erlang:apply(F, [process_args(Args)]);
apply_f(_) ->
    {error, "apply expects a function followed by arguments"}.

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
        "apply" => fun apply_f/1,
        "assoc" => fun types:assoc/1,
        "concat" => fun concat/1,
        "cons" => fun cons/1,
        "contains?" => fun types:contains_p/1,
        "count" => fun count/1,
        "dissoc" => fun types:dissoc/1,
        "empty?" => fun empty_q/1,
        "false?" => fun false_p/1,
        "first" => fun first/1,
        "get" => fun types:map_get/1,
        "hash-map" => fun types:hash_map/1,
        "keys" => fun types:map_keys/1,
        "keyword" => fun types:keyword/1,
        "keyword?" => fun types:keyword_p/1,
        "list" => fun types:list/1,
        "list?" => fun types:list_p/1,
        "map" => fun map_f/1,
        "map?" => fun types:map_p/1,
        "nil?" => fun nil_p/1,
        "nth" => fun nth/1,
        "pr-str" => fun pr_str/1,
        "println" => fun println/1,
        "prn" => fun prn/1,
        "read-string" => fun read_string/1,
        "rest" => fun rest/1,
        "sequential?" => fun types:sequential_p/1,
        "slurp" => fun slurp/1,
        "str" => fun str/1,
        "symbol" => fun types:symbol/1,
        "symbol?" => fun types:symbol_p/1,
        "throw" => fun mal_throw/1,
        "true?" => fun true_p/1,
        "vals" => fun types:map_values/1,
        "vector" => fun types:vector/1,
        "vector?" => fun types:vector_p/1
    },
    Env = env:new(undefined),
    SetEnv = fun(K, V) ->
        env:set(Env, {symbol, K}, types:func(V))
    end,
    maps:map(SetEnv, Builtins),
    Env.
