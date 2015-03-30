%%%
%%% Step 8: Macros
%%%

-module(step8_macros).

-export([main/1]).

main([File|Args]) ->
    Env = init(),
    env:set(Env, {symbol, "*ARGV*"}, {list, Args}),
    rep("(load-file \"" ++ File ++ "\")", Env);
main([]) ->
    Env = init(),
    env:set(Env, {symbol, "*ARGV*"}, {list, []}),
    loop(Env).

init() ->
    Env = core:ns(),
    eval(read("(def! not (fn* (a) (if a false true)))"), Env),
    eval(read("(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \")\")))))"), Env),
    eval(read("(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw \"odd number of forms to cond\")) (cons 'cond (rest (rest xs)))))))"), Env),
    eval(read("(defmacro! or (fn* (& xs) (if (empty? xs) nil (if (= 1 (count xs)) (first xs) `(let* (or_FIXME \~(first xs)) (if or_FIXME or_FIXME (or \~@(rest xs))))))))"), Env),
    Env.

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

eval(Value, Env) ->
    case Value of
        {list, _L1} ->
            case macroexpand(Value, Env) of
                {list, _L2} = List -> eval_list(List, Env);
                AST -> AST
            end;
        _ -> eval_ast(Value, Env)
    end.

eval_list({list, []}, _Env) ->
    [];
eval_list({list, [{symbol, "def!"}, {symbol, A1}, A2]}, Env) ->
    Result = eval(A2, Env),
    case Result of
        {error, _R1} -> Result;
        _ ->
            env:set(Env, {symbol, A1}, Result),
            Result
    end;
eval_list({list, [{symbol, "def!"}, _A1, _A2]}, _Env) ->
    error("def! called with non-symbol");
eval_list({list, [{symbol, "def!"}|_]}, _Env) ->
    error("def! requires exactly two arguments");
eval_list({list, [{symbol, "let*"}, A1, A2]}, Env) ->
    NewEnv = env:new(Env),
    let_star(NewEnv, A1),
    eval(A2, NewEnv);
eval_list({list, [{symbol, "let*"}|_]}, _Env) ->
    error("let* requires exactly two arguments");
eval_list({list, [{symbol, "do"}|Args]}, Env) ->
    eval_ast({list, lists:droplast(Args)}, Env),
    eval(lists:last(Args), Env);
eval_list({list, [{symbol, "if"}, Test, Consequent|Alternate]}, Env) ->
    case eval(Test, Env) of
        Cond when Cond == false orelse Cond == nil ->
            case Alternate of
                []  -> nil;
                [A] -> eval(A, Env);
                _   -> error("if takes 2 or 3 arguments")
            end;
        _ -> eval(Consequent, Env)
    end;
eval_list({list, [{symbol, "if"}|_]}, _Env) ->
    error("if requires test and consequent");
eval_list({list, [{symbol, "fn*"}, {vector, Binds}, Body]}, Env) ->
    {closure, fun eval/2, Binds, Body, Env};
eval_list({list, [{symbol, "fn*"}, {list, Binds}, Body]}, Env) ->
    {closure, fun eval/2, Binds, Body, Env};
eval_list({list, [{symbol, "fn*"}|_]}, _Env) ->
    error("fn* requires 2 arguments");
eval_list({list, [{symbol, "eval"}, AST]}, Env) ->
    % Must use the root environment so the variables set within the parsed
    % expression will be visible within the repl.
    eval(eval(AST, Env), env:root(Env));
eval_list({list, [{symbol, "eval"}|_]}, _Env) ->
    error("eval requires 1 argument");
eval_list({list, [{symbol, "quote"}, AST]}, _Env) ->
    AST;
eval_list({list, [{symbol, "quote"}|_]}, _Env) ->
    error("quote requires 1 argument");
eval_list({list, [{symbol, "quasiquote"}, AST]}, Env) ->
    eval(quasiquote(AST), Env);
eval_list({list, [{symbol, "quasiquote"}|_]}, _Env) ->
    error("quasiquote requires 1 argument");
eval_list({list, [{symbol, "defmacro!"}, {symbol, A1}, A2]}, Env) ->
    case eval(A2, Env) of
        {closure, _Eval, Binds, Body, CE} ->
            Result = {macro, Binds, Body, CE},
            env:set(Env, {symbol, A1}, Result),
            Result;
        Result -> env:set(Env, {symbol, A1}, Result), Result
    end,
    Result;
eval_list({list, [{symbol, "defmacro!"}, _A1, _A2]}, _Env) ->
    error("defmacro! called with non-symbol");
eval_list({list, [{symbol, "defmacro!"}|_]}, _Env) ->
    error("defmacro! requires exactly two arguments");
eval_list({list, [{symbol, "macroexpand"}, Macro]}, Env) ->
    macroexpand(Macro, Env);
eval_list({list, [{symbol, "macroexpand"}]}, _Env) ->
    error("macroexpand requires 1 argument");
eval_list({list, List}, Env) ->
    case eval_ast({list, List}, Env) of
        {list, [{closure, _Eval, Binds, Body, CE}|A]} ->
            % The args may be a single element or a list, so always make it
            % a list and then flatten it so it becomes a list.
            NewEnv = env:new(CE),
            env:bind(NewEnv, Binds, lists:flatten([A])),
            eval(Body, NewEnv);
        {list, [{function, F}|A]} -> erlang:apply(F, [A]);
        {list, [{error, Reason}]} -> {error, Reason};
        _ -> error("expected a list")
    end.

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

quasiquote({T, [{list, [{symbol, "splice-unquote"}, First]}|Rest]}) when T == list orelse T == vector ->
    % 3. if is_pair of first element of ast is true and the first element of
    %    first element of ast (ast[0][0]) is a symbol named "splice-unquote":
    %    return a new list containing: a symbol named "concat", the second element
    %    of first element of ast (ast[0][1]), and the result of calling quasiquote
    %    with the second through last element of ast.
    {list, [{symbol, "concat"}, First] ++ [quasiquote({list, Rest})]};
quasiquote({T, [{symbol, "splice-unquote"}]}) when T == list orelse T == vector ->
    {error, "splice-unquote requires an argument"};
quasiquote({T, [{symbol, "unquote"}, AST]}) when T == list orelse T == vector ->
    % 2. else if the first element of ast is a symbol named "unquote": return
    %    the second element of ast.
    AST;
quasiquote({T, [{symbol, "unquote"}|_]}) when T == list orelse T == vector ->
    {error, "unquote expects one argument"};
quasiquote({T, [First|Rest]}) when T == list orelse T == vector ->
    % 4. otherwise: return a new list containing: a symbol named "cons",
    %    the result of calling quasiquote on first element of ast (ast[0]),
    %    and result of calling quasiquote with the second through last
    %    element of ast.
    {list, [{symbol, "cons"}, quasiquote(First)] ++ [quasiquote({list, Rest})]};
quasiquote(AST) ->
    % 1. if is_pair of ast is false: return a new list containing:
    %    a symbol named "quote" and ast.
    {list, [{symbol, "quote"}, AST]}.

is_macro_call({list, [{symbol, Name}|_]}, Env) ->
    case env:find(Env, {symbol, Name}) of
        nil -> false;
        Env2 ->
            case env:get(Env2, {symbol, Name}) of
                {macro, _Binds, _Body, _ME} -> true;
                _ -> false
            end
    end;
is_macro_call(_AST, _Env) ->
    false.

macroexpand(AST, Env) ->
    case is_macro_call(AST, Env) of
        true ->
            {list, [Name|A]} = AST,
            {macro, Binds, Body, ME} = env:get(Env, Name),
            NewEnv = env:new(ME),
            env:bind(NewEnv, Binds, lists:flatten([A])),
            NewAST = eval(Body, NewEnv),
            macroexpand(NewAST, Env);
        false -> AST
    end.
