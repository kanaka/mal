%%%
%%% Step 0: REPL
%%%

-module(step0_repl).

-export([main/1]).

main(_) ->
    case io:get_line(standard_io, "user> ") of
        eof ->
            % break out of the loop
            io:format("~n"),
            ok;
        {error, Reason} ->
            io:format("Error reading input: ~p~n", [Reason]),
            exit(ioerr);
        Line ->
            io:format("~s~n", [print(eval(read(string:strip(Line, both, $\n))))]),
            main("")
    end.

read(String) ->
    String.

eval(String) ->
    String.

print(String) ->
    String.
