unit core;

{$H+} // Use AnsiString

interface

uses Classes,
     sysutils,
     fgl,
     mal_readline,
     mal_types,
     mal_func,
     mal_env,
     reader,
     printer;

type
    TCoreDict = specialize TFPGMap<string,TMalCallable>;

var
    EVAL : function (A: TMal; E: TEnv) : TMal;
    NS     : TCoreDict;

////////////////////////////////////////////////////////////

implementation

// General functions

function equal_Q(Args: TMalArray) : TMal;
begin
    equal_Q := wrap_tf(_equal_Q(Args[0], Args[1]));
end;

function throw(Args: TMalArray) : TMal;
begin
    raise TMalException.Create(Args[0]);
    throw := TMalNil.Create; // Not reached
end;

// Scalar functions

function nil_Q(Args: TMalArray) : TMal;
begin
    nil_Q := wrap_tf(Args[0] is TMalNil);
end;
function true_Q(Args: TMalArray) : TMal;
begin
    true_Q := wrap_tf(Args[0] is TMalTrue);
end;
function false_Q(Args: TMalArray) : TMal;
begin
    false_Q := wrap_tf(Args[0] is TMalFalse);
end;
function string_Q(Args: TMalArray) : TMal;
begin
    string_Q := wrap_tf(_string_Q(Args[0]));
end;
function symbol(Args: TMalArray) : TMal;
begin
    if Args[0] is TMalSymbol then
        symbol := Args[0]
    else if Args[0] is TMalString then
        symbol := TMalSymbol.Create((Args[0] as TMalString).Val)
    else
        raise Exception.Create('Invalid symbol call');
end;
function symbol_Q(Args: TMalArray) : TMal;
begin
    symbol_Q := wrap_tf(Args[0] is TMalSymbol);
end;
function keyword(Args: TMalArray) : TMal;
begin
    if ((Args[0] is TMalString) and not _string_Q(Args[0])) then
        keyword := Args[0]
    else if Args[0] is TMalString then
        keyword := TMalString.Create(#127 + (Args[0] as TMalString).Val)
    else
        raise Exception.Create('Invalid keyword call');
end;
function keyword_Q(Args: TMalArray) : TMal;
begin
    keyword_Q := wrap_tf((Args[0] is TMalString) and not _string_Q(Args[0]));
end;

// String functions

function do_pr_str(Args: TMalArray) : TMal;
begin
    do_pr_str := TMalString.Create(pr_str_array(Args, true, ' '));
end;
function str(Args: TMalArray) : TMal;
begin
    str := TMalString.Create(pr_str_array(Args, false, ''));
end;
function prn(Args: TMalArray) : TMal;
begin
    WriteLn(pr_str_array(Args, true, ' '));
    prn := TMalNil.Create;
end;
function println(Args: TMalArray) : TMal;
begin
    WriteLn(pr_str_array(Args, false, ' '));
    println := TMalNil.Create;
end;

function read_string(Args: TMalArray) : TMal;
begin
    read_string := read_str((Args[0] as TMalString).Val);
end;
function do_readline(Args: TMalArray) : TMal;
var
    Prompt : string;
    Line   : string;
begin
    Prompt := (Args[0] as TMalString).Val;
    try
        Line := _readline(Prompt);
        do_readline := TMalString.Create(Line);
    except
        On E : MalEOF do do_readline := TMalNil.Create;
    end;
end;
function slurp(Args: TMalArray) : TMal;
var
    StrL : TStringList;
begin
    StrL := TStringList.Create;
    StrL.LoadFromFile((Args[0] as TMalString).Val);
    slurp := TMalString.Create(StrL.Text);
end;

// Math functions

function lt(Args: TMalArray) : TMal;
begin
    lt := wrap_tf((Args[0] as TMalInt).Val < (Args[1] as TMalInt).Val);
end;
function lte(Args: TMalArray) : TMal;
begin
    lte := wrap_tf((Args[0] as TMalInt).Val <= (Args[1] as TMalInt).Val);
end;
function gt(Args: TMalArray) : TMal;
begin
    gt := wrap_tf((Args[0] as TMalInt).Val > (Args[1] as TMalInt).Val);
end;
function gte(Args: TMalArray) : TMal;
begin
    gte := wrap_tf((Args[0] as TMalInt).Val >= (Args[1] as TMalInt).Val);
end;

function add(Args: TMalArray) : TMal;
begin
    add := TMalInt.Create((Args[0] as TMalInt).Val +
                          (Args[1] as TMalInt).Val);
end;
function subtract(Args: TMalArray) : TMal;
begin
    subtract := TMalInt.Create((Args[0] as TMalInt).Val -
                               (Args[1] as TMalInt).Val);
end;
function multiply(Args: TMalArray) : TMal;
begin
    multiply := TMalInt.Create((Args[0] as TMalInt).Val *
                               (Args[1] as TMalInt).Val);
end;
function divide(Args: TMalArray) : TMal;
begin
    divide := TMalInt.Create((Args[0] as TMalInt).Val div
                             (Args[1] as TMalInt).Val);
end;
function time_ms(Args: TMalArray) : TMal;
begin
    time_ms := TMalInt.Create(Trunc(TimeStampToMSecs(DateTimeToTimeStamp(Now))));
end;

// Collection functions

function list(Args: TMalArray) : TMal;
begin
    list := TMalList.Create(Args);
end;
function list_Q(Args: TMalArray) : TMal;
begin
    list_Q := wrap_tf(Args[0].ClassType = TMalList);
end;
function vector(Args: TMalArray) : TMal;
begin
    vector := TMalVector.Create(Args);
end;
function vector_Q(Args: TMalArray) : TMal;
begin
    vector_Q := wrap_tf(Args[0].ClassType = TMalVector);
end;
function hash_map(Args: TMalArray) : TMal;
begin
    hash_map := TMalHashMap.Create(Args);
end;
function map_Q(Args: TMalArray) : TMal;
begin
    map_Q := wrap_tf(Args[0].ClassType = TMalHashMap);
end;
function assoc(Args: TMalArray) : TMal;
var
    OrigHM, NewHM : TMalHashMap;
begin
    OrigHM := (Args[0] as TMalHashMap);
    NewHM := TMalHashMap.Clone(OrigHM);
    assoc := NewHM.assoc_BANG(copy(Args, 1, Length(Args)));
end;
function dissoc(Args: TMalArray) : TMal;
var
    OrigHM, NewHM : TMalHashMap;
begin
    OrigHM := (Args[0] as TMalHashMap);
    NewHM := TMalHashMap.Clone(OrigHM);
    dissoc := NewHM.dissoc_BANG(copy(Args, 1, Length(Args)));
end;
function get(Args: TMalArray) : TMal;
var
    HM : TMalHashMap;
begin
    if Args[0] is TMalNil then Exit(TMalNil.Create);
    HM := (Args[0] as TMalHashMap);
    if HM.Val.IndexOf((Args[1] as TMalString).Val) >= 0 then
        get := HM.Val[(Args[1] as TMalString).Val]
    else
        get := TMalNil.Create;
end;
function contains_Q(Args: TMalArray) : TMal;
var
    HM : TMalHashMap;
begin
    if Args[0] is TMalNil then Exit(TMalFalse.Create);
    HM := (Args[0] as TMalHashMap);
    contains_Q := wrap_tf(HM.Val.IndexOf((Args[1] as TMalString).Val) >= 0);
end;
function keys(Args: TMalArray) : TMal;
var
    Dict : TMalDict;
    Arr  : TMalArray;
    I    : longint;
begin
    Dict := (Args[0] as TMalHashMap).Val;
    SetLength(Arr, Dict.Count);
    for I := 0 to Dict.Count-1 do
        Arr[I] := TMalString.Create(Dict.Keys[I]);
    keys := TMalList.Create(Arr);
end;
function vals(Args: TMalArray) : TMal;
var
    Dict : TMalDict;
    Arr  : TMalArray;
    I    : longint;
begin
    Dict := (Args[0] as TMalHashMap).Val;
    SetLength(Arr, Dict.Count);
    for I := 0 to Dict.Count-1 do
        Arr[I] := Dict[Dict.Keys[I]];
    vals := TMalList.Create(Arr);
end;


// Sequence functions

function sequential_Q(Args: TMalArray) : TMal;
begin
    sequential_Q := wrap_tf(_sequential_Q(Args[0]));
end;
function cons(Args: TMalArray) : TMal;
var
    Res, Src : TMalArray;
    I        : longint;
begin
    Src := (Args[1] as TMalList).Val;
    SetLength(Res, 1 + Length(Src));
    Res[0] := Args[0];
    for I := 1 to Length(Src) do
        Res[I] := Src[I-1];
    cons := TMalList.Create(Res);
end;
function do_concat(Args: TMalArray) : TMal;
var
    Res  : TMalArray;
    I    : longint;
begin
    SetLength(Res, 0);
    for I := 0 to Length(Args)-1 do
    begin
        Res := _concat(Res, (Args[I] as TMalList).Val);
    end;
    do_concat := TMalList.Create(Res);
end;
function nth(Args: TMalArray) : TMal;
var
    Arr : TMalArray;
    Idx : longint;
begin
    Arr := (Args[0] as TMalList).Val;
    Idx := (Args[1] as TMalInt).Val;
    if Idx >= Length(Arr) then
        raise Exception.Create('nth: index out of range')
    else
        nth := Arr[Idx];
end;
function first(Args: TMalArray) : TMal;
var
    Arr : TMalArray;
begin
    if Args[0] is TMalNil then Exit(TMalNil.Create);
    Arr := (Args[0] as TMalList).Val;
    if Length(Arr) = 0 then
        first := TMalNil.Create
    else
        first := (Args[0] as TMalList).Val[0];
end;
function rest(Args: TMalArray) : TMal;
begin
    if Args[0] is TMalNil then Exit(_list());
    rest := (Args[0] as TMalList).Rest();
end;

function empty_Q(Args: TMalArray) : TMal;
begin
    if Args[0] is TMalNil then
        empty_Q := TMalTrue.Create
    else if Args[0] is TMalList then
        empty_Q := wrap_tf(Length((Args[0] as TMalList).Val) = 0)
    else raise Exception.Create('invalid empty? call');
end;
function count(Args: TMalArray) : TMal;
begin
    if Args[0] is TMalNil then
        count := TMalInt.Create(0)
    else if Args[0] is TMalList then
        count := TMalInt.Create(Length((Args[0] as TMalList).Val))
    else raise Exception.Create('invalid count call');
end;

function map(Args: TMalArray) : TMal;
var
    Fn       : TMalFunc;
    FArgs    : TMalArray;
    Src, Res : TMalArray;
    I        : longint;
begin
    Fn := (Args[0] as TMalFunc);
    Src := (Args[1] as TMalList).Val;
    SetLength(FArgs, 1);
    SetLength(Res, Length(Src));
    if Fn.Ast = nil then
        for I := 0 to Length(Src)-1 do
        begin
            FArgs[0] := Src[I];
            Res[I] := Fn.Val(FArgs);
        end
    else
        for I := 0 to Length(Src)-1 do
        begin
            FArgs[0] := Src[I];
            Res[I] := EVAL(Fn.Ast, TEnv.Create(Fn.Env, Fn.Params, FArgs));
        end;
    map := TMalList.Create(Res);
end;
function apply(Args: TMalArray) : TMal;
var
    Fn       : TMalFunc;
    LastArgs : TMalArray;
    FArgs    : TMalArray;
    I        : longint;
begin
    Fn := (Args[0] as TMalFunc);
    LastArgs := (Args[Length(Args)-1] as TMalList).Val;
    SetLength(FArgs, Length(LastArgs) + Length(Args) - 2);
    for I := 0 to Length(Args)-3 do
        FArgs[I] := Args[I+1];
    for I := 0 to Length(LastArgs)-1 do
        FArgs[Length(Args)-2 + I] := LastArgs[I];
    if Fn.Ast = nil then
        apply := Fn.Val(FArgs)
    else
        apply := EVAL(Fn.Ast, TEnv.Create(Fn.Env, Fn.Params, FArgs));
end;

function conj(Args: TMalArray) : TMal;
var
    I    : longint;
    Vals : TMalArray;
begin
    if Args[0] is TMalVector then
        conj := TMalVector.Create(_concat((Args[0] as TMalList).Val,
                                          copy(Args, 1, Length(Args))))
    else if Args[0] is TMalList then
    begin
        SetLength(Vals, Length(Args)-1);
        for I := 1 to Length(Args)-1 do
            Vals[I-1] := Args[Length(Args) - I];
        conj := TMalList.Create(_concat(Vals, (Args[0] as TMalList).Val));
    end
    else
        raise Exception.Create('conj: called on non-sequence');
end;
function seq(Args: TMalArray) : TMal;
var
    Str : string;
    Arr : TMalArray;
    I   : longint;
begin
    if Args[0] is TMalVector then
    begin
        if Length((Args[0] as TMalVector).Val) = 0 then
            Exit(TMalNil.Create);
        seq := TMalList.Create((Args[0] as TMalVector).Val);
    end
    else if Args[0] is TMalList then
    begin
        if Length((Args[0] as TMalList).Val) = 0 then
            Exit(TMalNil.Create);
        seq := Args[0]
    end
    else if _string_Q(Args[0]) then
    begin
        Str := (Args[0] as TMalString).Val;
        if Length(Str) = 0 then
            Exit(TMalNil.Create);
        SetLength(Arr, Length(Str));
        for I := 0 to Length(Str) do
            Arr[I] := TMalString.Create(Str[I+1]);
        seq := TMalList.Create(Arr);
    end
    else if Args[0] is TMalNil then
    begin
        seq := Args[0];
    end
    else
        raise Exception.Create('seq: called on non-sequence');
end;


// Metadata functions

function meta(Args: TMalArray) : TMal;
begin
    if Args[0] is TMalFunc then
        meta := (Args[0] as TMalFunc).Meta
    else if Args[0] is TMalList then
        meta := (Args[0] as TMalList).Meta
    else if Args[0] is TMalHashMap then
        meta := (Args[0] as TMalHashMap).Meta
    else
        raise Exception.Create('meta not supported on ' + Args[0].ClassName);

    if meta = nil then
        meta := TMalNil.Create;
end;
function with_meta(Args: TMalArray) : TMal;
var
    Fn  : TMalFunc;
    Vec : TMalVector;
    Lst : TMalList;
    HM  : TMalHashMap;
begin
    if Args[0] is TMalFunc then
    begin
        Fn := TMalFunc.Clone(Args[0] as TMalFunc);
        Fn.Meta := Args[1];
        with_meta := Fn;
    end
    else if Args[0] is TMalVector then
    begin
        Vec := TMalVector.Clone(Args[0] as TMalVector);
        Vec.Meta := Args[1];
        with_meta := Vec;
    end
    else if Args[0] is TMalList then
    begin
        Lst := TMalList.Clone(Args[0] as TMalList);
        Lst.Meta := Args[1];
        with_meta := Lst;
    end
    else if Args[0] is TMalHashMap then
    begin
        HM := TMalHashMap.Clone(Args[0] as TMalHashMap);
        HM.Meta := Args[1];
        with_meta := HM;
    end
    else
        raise Exception.Create('with-meta call on non-mal function');
end;

// Atom functions

function atom(Args: TMalArray) : TMal;
begin
    atom := TMalAtom.Create(Args[0]);
end;
function atom_Q(Args: TMalArray) : TMal;
begin
    atom_Q := wrap_tf(Args[0] is TMalAtom);
end;
function deref(Args: TMalArray) : TMal;
begin
    deref := (Args[0] as TMalAtom).Val;
end;
function reset_BANG(Args: TMalArray) : TMal;
begin
    (Args[0] as TMalAtom).Val := Args[1];
    reset_BANG := Args[1];
end;

function swap_BANG(Args: TMalArray) : TMal;
var
    Atm   : TMalAtom;
    Fn    : TMalFunc;
    FArgs : TMalArray;
    I     : longint;
begin
    Atm := (Args[0] as TMalAtom);
    Fn := (Args[1] as TMalFunc);
    SetLength(FArgs, Length(Args)-1);
    FArgs[0] := Atm.Val;
    for I := 1 to Length(Args)-2 do
        FArgs[I] := Args[I+1];

    if Fn.Ast = nil then
        Atm.Val := Fn.Val(FArgs)
    else
        Atm.Val := EVAL(Fn.Ast, TEnv.Create(Fn.Env, Fn.Params, FArgs));
    swap_BANG := Atm.Val;
end;


////////////////////////////////////////////////////////////

initialization
begin
    NS := TCoreDict.Create;
    NS['='] := @equal_Q;
    NS['throw'] := @throw;

    NS['nil?'] := @nil_Q;
    NS['true?'] := @true_Q;
    NS['false?'] := @false_Q;
    NS['string?'] := @string_Q;
    NS['symbol'] := @symbol;
    NS['symbol?'] := @symbol_Q;
    NS['keyword'] := @keyword;
    NS['keyword?'] := @keyword_Q;

    NS['pr-str'] := @do_pr_str;
    NS['str'] := @str;
    NS['prn'] := @prn;
    NS['println'] := @println;
    NS['read-string'] := @read_string;
    NS['readline'] := @do_readline;
    NS['slurp'] := @slurp;

    NS['<'] := @lt;
    NS['<='] := @lte;
    NS['>'] := @gt;
    NS['>='] := @gte;
    NS['+'] := @add;
    NS['-'] := @subtract;
    NS['*'] := @multiply;
    NS['/'] := @divide;
    NS['time-ms'] := @time_ms;

    NS['list'] := @list;
    NS['list?'] := @list_Q;
    NS['vector'] := @vector;
    NS['vector?'] := @vector_Q;
    NS['hash-map'] := @hash_map;
    NS['map?'] := @map_Q;
    NS['assoc'] := @assoc;
    NS['dissoc'] := @dissoc;
    NS['get'] := @get;
    NS['contains?'] := @contains_Q;
    NS['keys'] := @keys;
    NS['vals'] := @vals;

    NS['sequential?'] := @sequential_Q;
    NS['cons'] := @cons;
    NS['concat'] := @do_concat;
    NS['nth'] := @nth;
    NS['first'] := @first;
    NS['rest'] := @rest;
    NS['empty?'] := @empty_Q;
    NS['count'] := @count;
    NS['apply'] := @apply;
    NS['map'] := @map;

    NS['conj'] := @conj;
    NS['seq'] := @seq;

    NS['meta'] := @meta;
    NS['with-meta'] := @with_meta;
    NS['atom'] := @atom;
    NS['atom?'] := @atom_Q;
    NS['deref'] := @deref;
    NS['reset!'] := @reset_BANG;
    NS['swap!'] := @swap_BANG;
end

end.
