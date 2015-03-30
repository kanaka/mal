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
            rep(string:strip(Line, both, $\n), Env),
            loop(Env)
    end.

rep(Input, Env) ->
    try eval(read(Input), Env) of
        Result -> print(Result)
    catch
        error:Reason -> io:format("error: ~s~n", [Reason])
    end.

read(Input) ->
    case reader:read_str(Input) of
        {ok, Value} -> Value;
        {error, Reason} -> error(Reason)
    end.

eval({list, []}, _Env) ->
    [];
eval({list, [{symbol, "def!"}, {symbol, A1}, A2]}, Env) ->
    Result = eval(A2, Env),
    env:set(Env, {symbol, A1}, Result),
    Result;
eval({list, [{symbol, "def!"}, _A1, _A2]}, _Env) ->
    error("def! called with non-symbol");
eval({list, [{symbol, "def!"}|_]}, _Env) ->
    error("def! requires exactly two arguments");
eval({list, [{symbol, "let*"}, A1, A2]}, Env) ->
    NewEnv = env:new(Env),
    let_star(NewEnv, A1),
    eval(A2, NewEnv);
eval({list, [{symbol, "let*"}|_]}, _Env) ->
    error("let* requires exactly two arguments");
eval({list, List}, Env) ->
    case eval_ast({list, List}, Env) of
        {list, [{function, F}|A]} -> erlang:apply(F, [A]);
        _ -> error("expected a list with a function")
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
    % (let* (p (+ 2 3) q (+ 2 p)) (+ p q))
    % ;=>12
    Bind = fun({Name, Expr}) ->
        case Name of
            {symbol, _Sym} -> env:set(Env, Name, eval(Expr, Env));
            _ -> error("let* with non-symbol binding")
        end
    end,
    case Bindings of
        {Type, Binds} when Type == list orelse Type == vector ->
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
