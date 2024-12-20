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
            print(rep(string:strip(Line, both, $\n), Env)),
            loop(Env)
    end.

rep(Input, Env) ->
    AST = read(Input),
    try eval(AST, Env) of
        none -> none;
        Result -> printer:pr_str(Result, true)
    catch
        error:Reason -> printer:pr_str({error, Reason}, true)
    end.

read(String) ->
    case reader:read_str(String) of
        {ok, Value} -> Value;
        {error, Reason} -> io:format("error: ~s~n", [Reason]), nil
    end.

eval({list, [], _Meta}=AST, _Env) ->
    AST;
eval({list, List, _Meta}, Env) ->
    case lists:map(fun(Elem) -> eval(Elem, Env) end, List) of
        [F|Args] -> erlang:apply(F, [Args]);
        _ -> {error, "expected a list"}
    end;
eval({symbol, Sym}, Env) ->
    case maps:is_key(Sym, Env) of
        true  -> maps:get(Sym, Env);
        false -> error(io_lib:format("'~s' not found", [Sym]))
    end;
eval({vector, V, Meta}, Env) ->
    {vector, lists:map(fun(Elem) -> eval(Elem, Env) end, V), Meta};
eval({map, M, Meta}, Env) ->
    {map, maps:map(fun(_Key, Val) -> eval(Val, Env) end, M), Meta};
eval(Value, _Env) ->
    Value.

print(none) ->
    % if nothing meaningful was entered, print nothing at all
    ok;
print(Value) ->
    io:format("~s~n", [Value]).
