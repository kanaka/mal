%%%
%%% Types and their functions
%%%

-module(types).
-compile(export_all).

list(Args) ->
	{list, Args, nil}.

list_p([Args]) ->
    case Args of
        {list, _L, _M} -> true;
        _ -> false
    end;
list_p([]) ->
    {error, "list? called with no arguments"};
list_p(_) ->
    {error, "list? expects one list argument"}.

func(Func) ->
    {function, Func, nil}.

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

string_p([{string, _S}]) ->
    true;
string_p([_A]) ->
    false;
string_p(_) ->
    {error, "string? takes a single argument"}.

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

vector_p([{vector, _V, _Meta}]) ->
    true;
vector_p([_]) ->
    false;
vector_p(_) ->
    {error, "vector? takes a single argument"}.

vector(Args) ->
    {vector, Args, nil}.

hash_map(Args) ->
    {map, reader:list_to_map(Args), nil}.

map_p([{map, _M, _Meta}]) ->
    true;
map_p([_]) ->
    false;
map_p(_) ->
    {error, "map? takes a single argument"}.

assoc([{map, Map, Meta}|Args]) ->
    case reader:list_to_map(Args) of
        {error, Reason} -> {error, Reason};
        Addend -> {map, maps:merge(Map, Addend), Meta}
    end;
assoc(_) ->
    {error, "assoc expects a map argument followed by pairs"}.

dissoc([{map, Map, Meta}|Keys]) ->
    {map, lists:foldl(fun(Key, AccIn) -> maps:remove(Key, AccIn) end, Map, Keys), Meta};
dissoc(_) ->
    {error, "dissoc expects a map argument followed by keys"}.

map_get([{map, Map, _Meta}, Key]) ->
    maps:get(Key, Map, nil);
map_get([_Thing1, _Thing2]) ->
    nil;
map_get(_) ->
    {error, "get expects a map argument followed by key"}.

contains_p([{map, Map, _Meta}, Key]) ->
    maps:is_key(Key, Map);
contains_p(_) ->
    {error, "contains? expects a map argument followed by key"}.

map_keys([{map, Map, _Meta}]) ->
    {list, maps:keys(Map), nil};
map_keys(_) ->
    {error, "keys expects a map argument"}.

map_values([{map, Map, _Meta}]) ->
    {list, maps:values(Map), nil};
map_values(_) ->
    {error, "vals expects a map argument"}.

sequential_p([{Type, _L, _M}]) when Type == list orelse  Type == vector ->
    true;
sequential_p([_]) ->
    false;
sequential_p(_) ->
    {error, "sequential? expects a single argument"}.

atom([Atom]) ->
    {atom, atom:new(Atom)};
atom(_) ->
    {error, "atom expects a single argument"}.

atom_p([{atom, _A}]) ->
    true;
atom_p([_]) ->
    false;
atom_p(_) ->
    {error, "atom? expects a single argument"}.

deref([{atom, Atom}]) ->
    atom:deref(Atom);
deref(_) ->
    {error, "deref expects a single atom argument"}.

reset([{atom, Atom}, Value]) ->
    atom:reset(Atom, Value);
reset(_) ->
    {error, "reset expects an atom and a value"}.

swap([{atom, Atom}, {closure, Eval, Binds, Body, Env, _MC}|Args]) ->
    NewEnv = env:new(Env),
    Values = [atom:deref(Atom) | Args],
    env:bind(NewEnv, Binds, Values),
    atom:reset(Atom, Eval(Body, NewEnv));
swap([{atom, Atom}, {function, F, _MF}|Args]) ->
    atom:reset(Atom, erlang:apply(F, [[atom:deref(Atom) | Args]]));
swap(_) ->
    {error, "atom expects an atom, function, and optional arguments"}.

meta([{T, _List, Meta}]) when T == list orelse T == vector orelse T == map ->
    Meta;
meta([{closure, _Eval, _Binds, _Body, _Env, Meta}]) ->
    Meta;
meta([{function, _Func, Meta}]) ->
    Meta;
meta(_) ->
    {error, "meta expects a single collection or function argument"}.

with_meta([{T, Seq, _M}, Meta]) when T == list orelse T == vector orelse T == map ->
    {T, Seq, Meta};
with_meta([{closure, Eval, Binds, Body, Env, _M}, Meta]) ->
    {closure, Eval, Binds, Body, Env, Meta};
with_meta([{function, Func, _Meta}, Meta]) ->
    {function, Func, Meta}.
