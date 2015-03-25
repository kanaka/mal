%%%
%%% Step 4: if, fn, do
%%%

-module(step4_if_fn_do).

-export([main/1]).

main(_) ->
    Env = core:ns(),
    % define the not function using mal itself
    eval(read("(def! not (fn* (a) (if a false true)))"), Env),
    loop(Env).

loop(Env) ->
    case io:get_line(standard_io, "user> ") of
        eof -> io:format("~n");
        {error, Reason} -> exit(Reason);
        Line ->
            rep(string:strip(Line, both, $\n), Env),
            loop(Env)
    end.

rep(Input, Env) ->
    try eval(read(Input), Env) of
        Result -> print(Result)
    catch
        throw:Reason -> io:format("error: ~s~n", [Reason])
    end.

read(Input) ->
    case reader:read_str(Input) of
        {ok, Value} -> Value;
        {error, Reason} -> throw(Reason)
    end.

eval({list, []}, _Env) ->
    [];
eval({list, [{symbol, "def!"}, {symbol, A1}, A2]}, Env) ->
    Result = eval(A2, Env),
    env:set(Env, {symbol, A1}, Result),
    Result;
eval({list, [{symbol, "def!"}, _A1, _A2]}, _Env) ->
    throw("def! called with non-symbol");
eval({list, [{symbol, "def!"}|_]}, _Env) ->
    throw("def! requires exactly two arguments");
eval({list, [{symbol, "let*"}, A1, A2]}, Env) ->
    NewEnv = env:new(Env),
    let_star(NewEnv, A1),
    eval(A2, NewEnv);
eval({list, [{symbol, "let*"}|_]}, _Env) ->
    throw("let* requires exactly two arguments");
eval({list, [{symbol, "do"}|Args]}, Env) ->
    {list, Results} = eval_ast({list, Args}, Env),
    lists:last(Results);
eval({list, [{symbol, "if"}, Test, Consequent|Alternate]}, Env) ->
    case eval(Test, Env) of
        Cond when Cond == false orelse Cond == nil ->
            case Alternate of
                []  -> nil;
                [A] -> eval(A, Env);
                _   -> throw("if takes 2 or 3 arguments")
            end;
        _ -> eval(Consequent, Env)
    end;
eval({list, [{symbol, "if"}|_]}, _Env) ->
    throw("if requires test and consequent");
eval({list, [{symbol, "fn*"}, {vector, Binds}, Body]}, Env) ->
    {closure, Binds, Body, Env};
eval({list, [{symbol, "fn*"}, {list, Binds}, Body]}, Env) ->
    {closure, Binds, Body, Env};
eval({list, [{symbol, "fn*"}|_]}, _Env) ->
    throw("fn* requires 2 arguments");
eval({list, List}, Env) ->
    case eval_ast({list, List}, Env) of
        {list, [{closure, Binds, Body, CE}|A]} ->
            % The args may be a single element or a list, so always make it
            % a list and then flatten it so it becomes a list.
            NewEnv = env:new(CE),
            env:bind(NewEnv, Binds, lists:flatten([A])),
            eval(Body, NewEnv);
        {list, [{function, F}|A]} -> erlang:apply(F, [A]);
        _ -> throw("expected a list")
    end;
eval(Value, Env) ->
    eval_ast(Value, Env).

eval_ast({symbol, _Sym}=Value, Env) ->
    env:get(Env, Value);
eval_ast({Type, Seq}, Env) when Type == list orelse Type == vector ->
    {Type, lists:map(fun(Elem) -> eval(Elem, Env) end, Seq)};
eval_ast({map, M}, Env) ->
    {map, maps:map(fun(_Key, Val) -> eval(Val, Env) end, M)};
eval_ast(Value, _Env) ->
    Value.

print(none) ->
    % if nothing meaningful was entered, print nothing at all
    ok;
print(Value) ->
    io:format("~s~n", [printer:pr_str(Value, true)]).

let_star(Env, Bindings) ->
    Bind = fun({Name, Expr}) ->
        case Name of
            {symbol, _Sym} -> env:set(Env, Name, eval(Expr, Env));
            _ -> throw("let* with non-symbol binding")
        end
    end,
    case Bindings of
        {Type, Binds} when Type == list orelse Type == vector ->
            case list_to_proplist(Binds) of
                {error, Reason} -> throw(Reason);
                Props -> lists:foreach(Bind, Props)
            end;
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
