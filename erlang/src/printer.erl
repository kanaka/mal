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
        {atom, Atom} ->
            AtomStr = pr_str(atom:deref(Atom), Readably),
            io_lib:format("(atom ~s)", [AtomStr]);
        {integer, Num} -> integer_to_list(Num);
        {string, String} when Readably == true -> escape_str(String);
        {string, String} when Readably == false -> String;
        {keyword, Keyword} -> [$:|Keyword];
        {symbol, Symbol} -> Symbol;
        {list, List, _Meta} -> pr_list(List, "(", ")", " ", Readably);
        {vector, Vector, _Meta} -> pr_list(Vector, "[", "]", " ", Readably);
        {map, Map, _Meta} -> pr_map(Map, Readably);
        {closure, _Eval, Binds, Body, _Env, _Meta} ->
            BindsStr = pr_str({list, Binds, nil}, Readably),
            BodyStr = pr_str(Body, Readably),
            io_lib:format("(fn* ~s ~s)", [BindsStr, BodyStr]);
        {function, _Func, _Meta} -> "#<function>";
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
    AppendKV = fun({Key, Value}, AccIn) ->
        AccIn ++ [Key, Value]
    end,
    Elements = lists:foldl(AppendKV, [], maps:to_list(Map)),
    pr_list(Elements, "{", "}", " ", Readably).

escape_str(String) ->
    Escape = fun(C, AccIn) ->
        case C of
            $"  -> [C, $\\|AccIn];
            $\\ -> [C, $\\|AccIn];
            $\n -> [$n, $\\|AccIn];
            _   -> [C|AccIn]
        end
    end,
    "\"" ++ lists:reverse(lists:foldl(Escape, [], String)) ++ "\"".
