%%%
%%% Types and their functions
%%%

-module(types).
-compile(export_all).

list(Args) ->
	{list, Args}.

list_p([Args]) ->
    case Args of
        {list, _L} -> true;
        _ -> false
    end;
list_p([]) ->
    {error, "list? called with no arguments"};
list_p(_) ->
    {error, "list? expects one list argument"}.

func(Func) ->
    {function, Func}.
