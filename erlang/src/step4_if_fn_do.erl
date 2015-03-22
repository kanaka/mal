%%%
%%% Step 4: if, fn, do
%%%

-module(step4_if_fn_do).

-export([main/1]).

main(_) ->
    % define the not function using mal itself
    AST = read("(def! not (fn* (a) (if a false true)))"),
    {_Result, Env} = eval(AST, core:ns()),
    loop(Env).

loop(Env) ->
    case io:get_line(standard_io, "user> ") of
        eof -> io:format("~n");
        {error, Reason} -> exit(Reason);
        Line -> loop(rep(string:strip(Line, both, $\n), Env))
    end.

rep(Input, Env) ->
    try eval(read(Input), Env) of
        {Result, E} -> print(Result), E
    catch
        throw:Reason -> io:format("error: ~s~n", [Reason]), Env
    end.

read(Input) ->
    case reader:read_str(Input) of
        {ok, Value} -> Value;
        {error, Reason} -> throw(Reason)
    end.

eval({list, []}, Env) ->
    {[], Env};
eval({list, [{symbol, "def!"}, A1, A2]}, Env) ->
    case A1 of
        {symbol, _A1} ->
            {Atwo, E2} = eval(A2, Env),
            {Atwo, env:set(E2, A1, Atwo)};
        _ -> throw("def! called with non-symbol")
    end;
eval({list, [{symbol, "def!"}|_]}, _Env) ->
    throw("def! requires exactly two arguments");
eval({list, [{symbol, "let*"}, A1, A2]}, Env) ->
    {Result, _E} = eval(A2, let_star(Env, A1)),
    {Result, Env};
eval({list, [{symbol, "let*"}|_]}, _Env) ->
    throw("let* requires exactly two arguments");
eval({list, [{symbol, "do"}|Args]}, Env) ->
    {{list, Results}, E2} = eval_ast({list, Args}, Env),
    {lists:last(Results), E2};
eval({list, [{symbol, "if"}, Test, Consequent|Alternate]}, Env) ->
    EvalAlternate = fun(Alt) ->
        case Alt of
            []  -> {nil, Env};
            [A] -> eval(A, Env);
            _   -> throw("if takes 2 or 3 arguments")
        end
    end,
    case eval(Test, Env) of
        {false, _E2} -> EvalAlternate(Alternate);
        {nil, _E2}   -> EvalAlternate(Alternate);
        _            -> eval(Consequent, Env)
    end;
eval({list, [{symbol, "if"}|_]}, _Env) ->
    throw("if requires test and consequent");
eval({list, [{symbol, "fn*"}, {vector, Binds}, Body]}, Env) ->
    {{closure, Binds, Body, Env}, Env};
eval({list, [{symbol, "fn*"}, {list, Binds}, Body]}, Env) ->
    {{closure, Binds, Body, Env}, Env};
eval({list, [{symbol, "fn*"}|_]}, _Env) ->
    throw("fn* requires 2 arguments");
eval({list, List}, Env) ->
    case eval_ast({list, List}, Env) of
        {{list, [{closure, Binds, Body, CE}|A]}, E2} ->
            % args may be a single element or a list, so
            % always make it a list and then flatten it
            CA = lists:flatten([A]),
            % hack to permit a closure to know its own
            % name, from the child environment
            Bound = env:bind(CE, Binds, CA),
            BoundWithFallback = env:fallback(Bound, E2),
            {Result, _E} = eval(Body, BoundWithFallback),
            % discard the environment from the closure
            {Result, Env};
        {{list, [{function, F}|A]}, E2} ->
            {erlang:apply(F, [A]), E2};
        _ -> throw("expected a list")
    end;
eval(Value, Env) ->
    eval_ast(Value, Env).

eval_ast(Value, Env) ->
    EvalList = fun(Elem, AccIn) ->
        {List, E} = AccIn,
        {Result, E2} = eval(Elem, E),
        {[Result|List], E2}
    end,
    EvalMap = fun(Key, Val, AccIn) ->
        {Map, E} = AccIn,
        {Result, E2} = eval(Val, E),
        {maps:put(Key, Result, Map), E2}
    end,
    case Value of
        {symbol, _Sym} -> {env:get(Env, Value), Env};
        {list, L} ->
            {Results, E2} = lists:foldl(EvalList, {[], Env}, L),
            {{list, lists:reverse(Results)}, E2};
        {vector, V} ->
            {Results, E2} = lists:foldl(EvalList, {[], Env}, V),
            {{vector, lists:reverse(Results)}, E2};
        {map, M} ->
            {Results, E2} = maps:fold(EvalMap, {#{}, Env}, M),
            {{map, Results}, E2};
        _ -> {Value, Env}
    end.

print(Value) ->
    case Value of
        none -> ok;  % if nothing meaningful was entered, print nothing at all
        _ -> io:format("~s~n", [printer:pr_str(Value, true)])
    end.

let_star(Env, Bindings) ->
    % (let* (p (+ 2 3) q (+ 2 p)) (+ p q))
    % ;=>12
    Bind = fun({Name, Expr}, E) ->
        case Name of
            {symbol, _Sym} ->
                {Value, E2} = eval(Expr, E),
                env:set(E2, Name, Value);
            _ -> throw("let* with non-symbol binding")
        end
    end,
    BindAll = fun(List) ->
        case list_to_proplist(List) of
            {error, Reason} -> throw(Reason);
            Props -> lists:foldl(Bind, Env, Props)
        end
    end,
    case Bindings of
        {list, Binds} -> BindAll(Binds);
        {vector, Binds} -> BindAll(Binds);
        _ -> throw("let* with non-list bindings")
    end.

list_to_proplist(L) ->
    list_to_proplist(L, []).

list_to_proplist([], AccIn) ->
    lists:reverse(AccIn);
list_to_proplist([_H], _AccIn) ->
    {error, "mismatch in let* name/value bindings"};
list_to_proplist([K,V|T], AccIn) ->
    list_to_proplist(T, [{K, V}|AccIn]).
