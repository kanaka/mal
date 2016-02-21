%%%
%%% Reader
%%%

-module(reader).

-export([read_str/1, list_to_map/1]).

-record(reader, {
    tokens=[],  % the input tokens remaining
    tree        % the subtree parsed by a read_* function
}).

-spec read_str(string()) -> {ok, term()} | {error, term()}.
read_str(Input) ->
    case tokenize(Input) of
        {ok, []} -> {ok, none};
        {ok, Tokens} ->
            case read_form(#reader{tokens=Tokens}) of
                % extract the final result of parsing
                {ok, Reader} -> {ok, Reader#reader.tree};
                {error, Reason} -> {error, Reason}
            end;
        {error, Reason} -> {error, Reason}
    end.

-spec read_form(#reader{}) -> {ok, #reader{}} | {error, term()}.
read_form(Reader) ->
    Token = peek(Reader),
    case Token of
        close_list       -> {error, "unexected ')'"};
        close_vector     -> {error, "unexected ']'"};
        close_map        -> {error, "unexected '}'"};
        open_list        -> read_list(Reader);
        open_vector      -> read_vector(Reader);
        open_map         -> read_map(Reader);
        quote            -> read_quoted(Reader, Token);
        quasiquote       -> read_quoted(Reader, Token);
        unquote          -> read_quoted(Reader, Token);
        'splice-unquote' -> read_quoted(Reader, Token);
        deref            -> read_quoted(Reader, Token);
        'with-meta'      -> read_meta(Reader);
        _ -> read_atom(Reader)
    end.

read_list(Reader) ->
    read_seq(Reader, $), open_list, close_list, list).

read_vector(Reader) ->
    % Erlang has no array/vector type, so just use list
    read_seq(Reader, $], open_vector, close_vector, vector).

read_map(Reader) ->
    case read_seq(Reader, $}, open_map, close_map, map) of
        {ok, Reader1} ->
            {map, Map, Meta} = Reader1#reader.tree,
            case list_to_map(Map) of
                {error, Reason} -> {error, Reason};
                NewMap ->
                    Tokens = Reader1#reader.tokens,
                    {ok, #reader{tokens=Tokens, tree={map, NewMap, Meta}}}
            end;
        {error, Reason} -> {error, Reason}
    end.

read_seq(Reader, CloseChar, OpenDelim, CloseDelim, Type) ->
    {First, Reader1} = next(Reader),
    case First of
        OpenDelim ->
            case read_seq_tail(Reader1, CloseChar, CloseDelim, []) of
                {ok, Reader2} ->
                    % prepend our type tag to the result
                    Result = {Type, Reader2#reader.tree, nil},
                    Reader3 = #reader{tokens=Reader2#reader.tokens, tree=Result},
                    {ok, Reader3};
                {error, Reason} -> {error, Reason}
            end;
        Bogey -> {error, io_lib:format("error in read_seq, expected ~p but got ~p",
                                       [OpenDelim, Bogey])}
    end.

read_seq_tail(Reader, CloseChar, CloseDelim, AccIn) ->
    Token = peek(Reader),
    case Token of
        [] -> {error, io_lib:format("expected '~c', got EOF", [CloseChar])};
        CloseDelim ->
            {_Token, Reader1} = next(Reader),
            Reader2 = #reader{tokens=Reader1#reader.tokens, tree=lists:reverse(AccIn)},
            {ok, Reader2};
        _ ->
            case read_form(Reader) of
                {ok, Reader3} ->
                    read_seq_tail(Reader3, CloseChar, CloseDelim, [Reader3#reader.tree|AccIn]);
                {error, Reason} -> {error, Reason}
            end
    end.

% Convert a list of key/value pairs into a map. The elements are not
% tuples; the keys are the odd numbered members, and the values are the
% even numbered members. Fails if list has an odd number of members.
list_to_map(L) ->
    list_to_map(L, #{}).

list_to_map([], AccIn) ->
    AccIn;
list_to_map([_H], _AccIn) ->
    {error, "odd number of hash-map keys/values"};
list_to_map([K,V|T], AccIn) ->
    list_to_map(T, maps:put(K, V, AccIn)).

% Convert syntactic sugar into normalized form (e.g. ` => (quasiquote)).
read_quoted(Reader, Token) ->
    % discard the quoted token
    {_T, Reader1} = next(Reader),
    case read_form(Reader1) of
        {ok, Reader2} ->
            Result = {list, [{symbol, atom_to_list(Token)}, Reader2#reader.tree], nil},
            {ok, #reader{tokens=Reader2#reader.tokens, tree=Result}};
        {error, Reason} -> {error, Reason}
    end.

read_meta(Reader) ->
    % discard the meta token
    {_T, Reader1} = next(Reader),
    case read_form(Reader1) of
        {ok, Reader2} ->
            M = Reader2#reader.tree,
            case read_form(Reader2) of
                {ok, Reader3} ->
                    X = Reader3#reader.tree,
                    Result = {list, [{symbol, "with-meta"}, X, M], nil},
                    {ok, #reader{tokens=Reader3#reader.tokens, tree=Result}};
                {error, Reason} -> {error, Reason}
            end;
        {error, Reason} -> {error, Reason}
    end.

read_atom(Reader) ->
    {Token, Reader1} = next(Reader),
    Result = case Token of
        {integer, Value} -> {integer, list_to_integer(Value)};
        {string, _String} -> Token;
        {keyword, _Keyword} -> Token;
        {symbol, Symbol} ->
            case Symbol of
                "true" -> true;
                "false" -> false;
                "nil" -> nil;
                _ -> Token
            end
    end,
    {ok, #reader{tokens=Reader1#reader.tokens, tree=Result}}.

peek(Reader) ->
    case Reader#reader.tokens of
        [] -> [];
        [H|_T] -> H
    end.

next(Reader) ->
    [H|NewTokens] = Reader#reader.tokens,
    {H, #reader{tokens=NewTokens}}.

-spec tokenize(string()) -> {ok, [term()]} | {error, term()}.
tokenize(Input) ->
    tokenize(Input, []).

-spec tokenize(string(), [term()]) -> {ok, [term()]} | {error, term()}.
tokenize(Input, Tokens) ->
    case lex_single(Input) of
        eof -> {ok, lists:reverse(Tokens)};
        {error, Reason} -> {error, Reason};
        {ignored, Rest} -> tokenize(Rest, Tokens);
        {Token, Rest} -> tokenize(Rest, [Token|Tokens])
    end.

lex_single([]) ->
    eof;
lex_single([Char|Rest]) ->
    case Char of
        $( -> {open_list, Rest};
        $) -> {close_list, Rest};
        $[ -> {open_vector, Rest};
        $] -> {close_vector, Rest};
        ${ -> {open_map, Rest};
        $} -> {close_map, Rest};
        $" -> lex_string(Rest, []);
        $; -> lex_comment(Rest);
        $: -> lex_symbol(Rest, keyword);
        $' -> {quote, Rest};
        $` -> {quasiquote, Rest};
        $~ -> lex_unquote(Rest);
        $@ -> {deref, Rest};
        $^ -> {'with-meta', Rest};
        N when N >= $0, N =< $9 -> lex_number(Rest, [Char]);
        S when S == 32; S == $,; S == $\r; S == $\n; S == $\t -> lex_spaces(Rest);
        $\\ -> {error, io_lib:format("bare escape literal ~c~c", [Char, hd(Rest)])};
        $. -> {error, "bare dot (.) not supported"};
        _ -> lex_symbol([Char|Rest], symbol)
    end.

lex_comment([]) ->
    {ignored, []};
lex_comment([C|Rest]) when C == $\r; C == $\n ->
    {ignored, Rest};
lex_comment([_C|Rest]) ->
    lex_comment(Rest).

lex_spaces([C|Rest]) when C == 32; C == $,; C == $\r; C == $\n; C == $\t ->
    lex_spaces(Rest);
lex_spaces(Rest) ->
    {ignored, Rest}.

lex_string([], _String) ->
    {error, "expected '\"', got EOF"};
lex_string([$\\,Escaped|Rest], String) ->
    % unescape the string while building it
    case Escaped of
        []  -> {error, "end of string reached in escape"};
        $n  -> lex_string(Rest, [$\n|String]);
        _   -> lex_string(Rest, [Escaped|String])
    end;
lex_string([$"|Rest], String) ->
    {{string, lists:reverse(String)}, Rest};
lex_string([C|Rest], String) ->
    lex_string(Rest, [C|String]).

lex_number([N|Rest], Number) when N >= $0, N =< $9 ->
    lex_number(Rest, [N|Number]);
lex_number(Rest, Number) ->
    {{integer, lists:reverse(Number)}, Rest}.

% Lex the remainder of either a keyword or a symbol. The Type is used as
% the tag for the returned tuple (e.g. the atoms keyword or symbol).
lex_symbol(Input, Type) ->
    IsSymbol = fun(C) ->
        is_letter(C) orelse is_digit(C) orelse is_symbol(C)
    end,
    Symbol = lists:takewhile(IsSymbol, Input),
    case Symbol of
        [] -> {error, io_lib:format("invalid symbol: ~10s", [Input])};
        _ -> {{Type, Symbol}, lists:sublist(Input, length(Symbol) + 1, length(Input))}
    end.

is_digit(C) ->
    C >= $0 andalso C =< $9.

is_letter(C) ->
    C >= $a andalso C =< $z orelse C >= $A andalso C =< $Z.

is_symbol(C) ->
    lists:member(C, "!#$%&*+-/:<=>?@^_|\~").

lex_unquote([$@|Rest]) ->
    {'splice-unquote', Rest};
lex_unquote(Rest) ->
    {unquote, Rest}.
