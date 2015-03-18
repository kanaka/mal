%%%
%%% Step 2: eval
%%%

-module(step2_eval).

-export([main/1]).

main(_) ->
    Env = core:ns(),
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
        throw:Reason -> io:format("error: ~s~n", [Reason])
    end.

read(String) ->
    case reader:read_str(String) of
        {ok, Value} -> Value;
        {error, Reason} -> io:format("error: ~s~n", [Reason]), nil
    end.

eval(Value, Env) ->
    case Value of
        {list, _List} ->
            case eval_ast(Value, Env) of
                {list, [F|Args]} -> erlang:apply(F, [Args]);
                _ -> {error, "expected a list"}
            end;
        _ -> eval_ast(Value, Env)
    end.

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
                false -> throw(io_lib:format("'~s' not found", [Sym]))
            end;
        {list, L}   -> {list, lists:map(EvalList, L)};
        {vector, V} -> {vector, lists:map(EvalList, V)};
        {map, M}    -> {map, maps:map(EvalMap, M)};
        _           -> Value
    end.

print(Value) ->
    case Value of
        none -> ok;  % if nothing meaningful was entered, print nothing at all
        _ -> io:format("~s~n", [printer:pr_str(Value, true)])
    end.
