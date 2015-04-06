%%%
%%% Step 1: read/print
%%%

-module(step1_read_print).

-export([main/1]).

main(_) ->
    case io:get_line(standard_io, "user> ") of
        eof ->
            % break out of the loop
            io:format("~n"),
            ok;
        {error, Reason} ->
            io:format("Error reading input: ~s~n", [Reason]),
            exit(ioerr);
        Line ->
            print(eval(read(string:strip(Line, both, $\n)))),
            main("")
    end.

read(String) ->
    case reader:read_str(String) of
        {ok, Value} -> Value;
        {error, Reason} -> io:format("error: ~s~n", [Reason]), nil
    end.

eval(Value) ->
    Value.

print(none) ->
    % if nothing meaningful was entered, print nothing at all
    ok;
print(Value) ->
    io:format("~s~n", [printer:pr_str(Value, true)]).
