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

symbol_p([{symbol, _S}]) ->
    true;
symbol_p([_A]) ->
    false;
symbol_p(_) ->
    {error, "symbol? takes a single argument"}.

symbol([{string, Name}]) ->
    {symbol, Name};
symbol(_) ->
    {error, "symbol expects a single string argument"}.

keyword_p([{keyword, _K}]) ->
    true;
keyword_p([_A]) ->
    false;
keyword_p(_) ->
    {error, "keyword? takes a single argument"}.

keyword([{string, Name}]) ->
    {keyword, Name};
keyword(_) ->
    {error, "keyword expects a single string argument"}.

vector_p([{vector, _V}]) ->
    true;
vector_p([_]) ->
    false;
vector_p(_) ->
    {error, "vector? takes a single argument"}.

vector(Args) ->
    {vector, Args}.

hash_map(Args) ->
    {map, reader:list_to_map(Args)}.

map_p([{map, _M}]) ->
    true;
map_p([_]) ->
    false;
map_p(_) ->
    {error, "map? takes a single argument"}.

assoc([{map, Map}|Args]) ->
    Addend = reader:list_to_map(Args),
    {map, maps:merge(Map, Addend)};
assoc(A) ->
    io:format("assoc: ~p~n", [A]),
    {error, "assoc expects a map argument followed by pairs"}.

dissoc([{map, Map}|Keys]) ->
    {map, lists:foldl(fun(Key, AccIn) -> maps:remove(Key, AccIn) end, Map, Keys)};
dissoc(_) ->
    {error, "dissoc expects a map argument followed by keys"}.

map_get([{map, Map}, Key]) ->
    maps:get(Key, Map, nil);
map_get([_Thing1, _Thing2]) ->
    nil;
map_get(_) ->
    {error, "get expects a map argument followed by key"}.

contains_p([{map, Map}, Key]) ->
    maps:is_key(Key, Map);
contains_p(_) ->
    {error, "contains? expects a map argument followed by key"}.

map_keys([{map, Map}]) ->
    {list, maps:keys(Map)};
map_keys(_) ->
    {error, "keys expects a map argument"}.

map_values([{map, Map}]) ->
    {list, maps:values(Map)};
map_values(_) ->
    {error, "vals expects a map argument"}.

sequential_p([{Type, _L}]) when Type == list orelse  Type == vector ->
    true;
sequential_p([_]) ->
    false;
sequential_p(_) ->
    {error, "sequential? expects a single argument"}.
