%%%
%%% Printer
%%%

-module(printer).

-export([pr_str/2, pr_list/5]).

-spec pr_str(term(), true|false) -> string().
pr_str(Value, Readably) ->
	case Value of
		nil -> "nil";
		true -> "true";
		false -> "false";
        {integer, Num} -> integer_to_list(Num);
        {string, String} when Readably == true -> escape_str(String);
        {string, String} when Readably == false -> String;
        {keyword, Keyword} -> [$:|Keyword];
        {symbol, Symbol} -> Symbol;
        {list, List} -> pr_list(List, "(", ")", " ", Readably);
        {vector, Vector} -> pr_list(Vector, "[", "]", " ", Readably);
        {map, Map} -> pr_map(Map, Readably);
        {closure, _Binds, _Body, _Env} -> "#<function>";
        {function, _Func} -> "#<builtin>";
        {macro, _Binds, _Body, _Env} -> "#<macro>";
        {error, Reason} -> io_lib:format("error: ~s", [Reason])
	end.

-spec pr_list([term()], string(), string(), string(), boolean()) -> string().
pr_list(Seq, Start, End, Join, Readably) ->
    Print = fun(Elem) ->
        pr_str(Elem, Readably)
    end,
    L = string:join(lists:map(Print, Seq), Join),
    Start ++ L ++ End.

pr_map(Map, Readably) ->
    PrintKV = fun({Key, Value}) ->
        KS = pr_str(Key, Readably),
        VS = pr_str(Value, Readably),
        KS ++ " " ++ VS
    end,
    L = string:join(lists:map(PrintKV, maps:to_list(Map)), " "),
    io_lib:format("{~s}", [L]).

escape_str(String) ->
    Escape = fun(C, AccIn) ->
        case C of
            $"  -> [C, $\\|AccIn];
            $\\ -> [C, $\\|AccIn];
            $\n -> [C, $\\|AccIn];
            _   -> [C|AccIn]
        end
    end,
    "\"" ++ lists:reverse(lists:foldl(Escape, [], String)) ++ "\"".
