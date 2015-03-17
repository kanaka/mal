%%%
%%% Printer
%%%

-module(printer).

-export([pr_str/2]).

-spec pr_str(term(), true|false) -> string().
pr_str(Value, Readably) ->
	case Value of
		nil -> "nil";
		true -> "true";
		false -> "false";
        {integer, Num} -> integer_to_list(Num);
        {string, String} -> io_lib:format("~s", [escape_str(String, Readably)]);
        {keyword, Keyword} -> io_lib:format("~s", [[$:|Keyword]]);
        {symbol, Symbol} -> io_lib:format("~s", [Symbol]);
        {list, List} -> pr_list(List, $(, $), Readably);
        {vector, Vector} -> pr_list(Vector, $[, $], Readably);
        {map, Map} -> pr_map(Map, Readably)
	end.

pr_list(Seq, Start, End, Readably) ->
    Print = fun(Elem) ->
        pr_str(Elem, Readably)
    end,
    L = string:join(lists:map(Print, Seq), " "),
    io_lib:format("~c~s~c", [Start, L, End]).

pr_map(Map, Readably) ->
    PrintKV = fun({Key, Value}) ->
        KS = pr_str(Key, Readably),
        VS = pr_str(Value, Readably),
        KS ++ " " ++ VS
    end,
    L = string:join(lists:map(PrintKV, maps:to_list(Map)), " "),
    io_lib:format("{~s}", [L]).

escape_str(String, false) ->
    "\"" ++ String ++ "\"";
escape_str(String, true) ->
    Escape = fun(C, AccIn) ->
        case C of
            $"  -> [C, $\\|AccIn];
            $\n -> [C, $\\|AccIn];
            _   -> [C|AccIn]
        end
    end,
    escape_str(lists:reverse(lists:foldl(Escape, [], String)), false).
