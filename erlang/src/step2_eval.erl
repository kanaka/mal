%%%
%%% Step 2: eval
%%%

-module(step2_eval).

-export([main/1]).

main(_) ->
    Env = #{
        "+" => fun core:int_add/1,
        "-" => fun core:int_sub/1,
        "*" => fun core:int_mul/1,
        "/" => fun core:int_div/1
    },
    loop(Env).

loop(Env) ->
    case io:get_line(standard_io, "user> ") of
        eof ->
            % break out of the loop
            io:format("~n"),
            ok;
        {error, Reason} ->
            io:format("Error reading input: ~s~n", [Reason]),
            exit(ioerr);
        Line ->
            rep(string:strip(Line, both, $\n), Env),
            loop(Env)
    end.

rep(Input, Env) ->
    AST = read(Input),
    try eval(AST, Env) of
        Result -> print(Result)
    catch
        error:Reason -> io:format("error: ~s~n", [Reason])
    end.

read(String) ->
    case reader:read_str(String) of
        {ok, Value} -> Value;
        {error, Reason} -> io:format("error: ~s~n", [Reason]), nil
    end.

eval({list, List, Meta}, Env) ->
    case eval_ast({list, List, Meta}, Env) of
        {list, [F|Args], _M} -> erlang:apply(F, [Args]);
        _ -> {error, "expected a list"}
    end;
eval(Value, Env) ->
    eval_ast(Value, Env).

eval_ast(Value, Env) ->
    EvalList = fun(Elem) ->
        eval(Elem, Env)
    end,
    EvalMap = fun(_Key, Val) ->
        eval(Val, Env)
    end,
    case Value of
        {symbol, Sym} ->
            case maps:is_key(Sym, Env) of
                true  -> maps:get(Sym, Env);
                false -> error(io_lib:format("'~s' not found", [Sym]))
            end;
        {list, L, Meta}   -> {list, lists:map(EvalList, L), Meta};
        {vector, V, Meta} -> {vector, lists:map(EvalList, V), Meta};
        {map, M, Meta}    -> {map, maps:map(EvalMap, M), Meta};
        _ -> Value
    end.

print(none) ->
    % if nothing meaningful was entered, print nothing at all
    ok;
print(Value) ->
    io:format("~s~n", [printer:pr_str(Value, true)]).
