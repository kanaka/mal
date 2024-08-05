%%%
%%% Step 3: env
%%%

-module(step3_env).

-export([main/1]).

main(_) ->
    loop(core:ns()).

loop(Env) ->
    case io:get_line(standard_io, "user> ") of
        eof -> io:format("~n");
        {error, Reason} -> exit(Reason);
        Line ->
            print(rep(string:strip(Line, both, $\n), Env)),
            loop(Env)
    end.

rep(Input, Env) ->
    try eval(read(Input), Env) of
        none -> none;
        Result -> printer:pr_str(Result, true)
    catch
        error:Reason -> printer:pr_str({error, Reason}, true)
    end.

read(Input) ->
    case reader:read_str(Input) of
        {ok, Value} -> Value;
        {error, Reason} -> error(Reason)
    end.

eval(Value, Env) ->
    case env:find(Env, {symbol, "DEBUG-EVAL"}) of
        nil -> none;
        Env2 ->
            case env:get(Env2, {symbol, "DEBUG-EVAL"}) of
                Cond when Cond == false orelse Cond == nil -> none;
                _ -> io:format("EVAL: ~s~n", [printer:pr_str(Value, true)])
            end
    end,
    eval_ast(Value, Env).

eval_list({list, [], _Meta}=AST, _Env) ->
    AST;
eval_list({list, [{symbol, "def!"}, {symbol, A1}, A2], _Meta}, Env) ->
    Result = eval(A2, Env),
    env:set(Env, {symbol, A1}, Result),
    Result;
eval_list({list, [{symbol, "def!"}, _A1, _A2], _Meta}, _Env) ->
    error("def! called with non-symbol");
eval_list({list, [{symbol, "def!"}|_], _Meta}, _Env) ->
    error("def! requires exactly two arguments");
eval_list({list, [{symbol, "let*"}, A1, A2], _Meta}, Env) ->
    NewEnv = env:new(Env),
    let_star(NewEnv, A1),
    eval(A2, NewEnv);
eval_list({list, [{symbol, "let*"}|_], _Meta}, _Env) ->
    error("let* requires exactly two arguments");
eval_list({list, [A0 | Args], _Meta}, Env) ->
    case eval(A0, Env) of
        {function, F, _MF} ->
            A = lists:map(fun(Elem) -> eval(Elem, Env) end, Args),
            erlang:apply(F, [A]);
        {error, Reason} -> {error, Reason}
    end.

eval_ast({symbol, _Sym}=Value, Env) ->
    env:get(Env, Value);
eval_ast({list, Seq, Meta}, Env) ->
    eval_list({list, Seq, Meta}, Env);
eval_ast({vector, Seq, _Meta}, Env) ->
    {vector, lists:map(fun(Elem) -> eval(Elem, Env) end, Seq), nil};
eval_ast({map, M, _Meta}, Env) ->
    {map, maps:map(fun(_Key, Val) -> eval(Val, Env) end, M), nil};
eval_ast(Value, _Env) ->
    Value.

print(none) ->
    % if nothing meaningful was entered, print nothing at all
    ok;
print(Value) ->
    io:format("~s~n", [Value]).

let_star(Env, Bindings) ->
    % (let* (p (+ 2 3) q (+ 2 p)) (+ p q))
    % ;=>12
    Bind = fun({Name, Expr}) ->
        case Name of
            {symbol, _Sym} -> env:set(Env, Name, eval(Expr, Env));
            _ -> error("let* with non-symbol binding")
        end
    end,
    case Bindings of
        {Type, Binds, _Meta} when Type == list orelse Type == vector ->
            case list_to_proplist(Binds) of
                {error, Reason} -> error(Reason);
                Props -> lists:foreach(Bind, Props)
            end;
        _ -> error("let* with non-list bindings")
    end.

list_to_proplist(L) ->
    list_to_proplist(L, []).

list_to_proplist([], AccIn) ->
    lists:reverse(AccIn);
list_to_proplist([_H], _AccIn) ->
    {error, "mismatch in let* name/value bindings"};
list_to_proplist([K,V|T], AccIn) ->
    list_to_proplist(T, [{K, V}|AccIn]).
