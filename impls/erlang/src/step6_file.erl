%%%
%%% Step 6: File and evil
%%%

-module(step6_file).

-export([main/1]).

main([File|Args]) ->
    Env = init(),
    env:set(Env, {symbol, "*ARGV*"}, {list, [{string,Arg} || Arg <- Args], nil}),
    rep("(load-file \"" ++ File ++ "\")", Env);
main([]) ->
    Env = init(),
    env:set(Env, {symbol, "*ARGV*"}, {list, [], nil}),
    loop(Env).

init() ->
    Env = core:ns(),
    % define the load-file and not functions using mal itself
    eval(read("(def! not (fn* (a) (if a false true)))"), Env),
    eval(read("(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \"\nnil)\")))))"), Env),
    Env.

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
eval_list({list, [{symbol, "do"}|Args], _Meta}, Env) ->
    lists:map(fun(Elem) -> eval(Elem, Env) end, lists:droplast(Args)),
    eval(lists:last(Args), Env);
eval_list({list, [{symbol, "if"}, Test, Consequent|Alternate], _Meta}, Env) ->
    case eval(Test, Env) of
        Cond when Cond == false orelse Cond == nil ->
            case Alternate of
                []  -> nil;
                [A] -> eval(A, Env);
                _   -> error("if takes 2 or 3 arguments")
            end;
        _ -> eval(Consequent, Env)
    end;
eval_list({list, [{symbol, "if"}|_], _Meta}, _Env) ->
    error("if requires test and consequent");
eval_list({list, [{symbol, "fn*"}, {vector, Binds, _M1}, Body], _Meta}, Env) ->
    {closure, fun eval/2, Binds, Body, Env,  nil};
eval_list({list, [{symbol, "fn*"}, {list, Binds, _M1}, Body], _Meta}, Env) ->
    {closure, fun eval/2, Binds, Body, Env, nil};
eval_list({list, [{symbol, "fn*"}|_], _Meta}, _Env) ->
    error("fn* requires 2 arguments");
eval_list({list, [{symbol, "eval"}, AST], _Meta}, Env) ->
    % Must use the root environment so the variables set within the parsed
    % expression will be visible within the repl.
    eval(eval(AST, Env), env:root(Env));
eval_list({list, [{symbol, "eval"}|_], _Meta}, _Env) ->
    error("eval requires 1 argument");
eval_list({list, [A0 | Args], _Meta}, Env) ->
    case eval(A0, Env) of
        {closure, _Eval, Binds, Body, CE, _MC} ->
            % The args may be a single element or a list, so always make it
            % a list and then flatten it so it becomes a list.
            A = lists:map(fun(Elem) -> eval(Elem, Env) end, Args),
            NewEnv = env:new(CE),
            env:bind(NewEnv, Binds, lists:flatten([A])),
            eval(Body, NewEnv);
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
