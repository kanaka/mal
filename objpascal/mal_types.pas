unit mal_types;

{$H+} // Use AnsiString

interface

uses sysutils,
     fgl;

// Ancestor of all Mal types

type TMal = class(TObject);


// Some general type definitions

type
    TMalArray = array of TMal;
    // TODO: use http://bugs.freepascal.org/view.php?id=27206 when
    // incorporated into FPC
    TMalDict = specialize TFPGMap<string,TMal>;

type TMalException = class(Exception)
    public
        Val: TMal;

        constructor Create(V : TMal);
end;


// Mal types

type TMalNil = class(TMal);
type TMalTrue = class(TMal);
type TMalFalse = class(TMal);

type TMalInt = class(TMal)
    public
        Val: int64;

        constructor Create(V : int64);
end;

type TMalString = class(TMal)
    public
        Val: string;

        constructor Create(V : string);
end;

type TMalSymbol = class(TMal)
    public
        Val: string;

        constructor Create(V : string);
end;


type TMalList = class(TMal)
    public
        Val:  TMalArray;
        Meta: TMal;

        constructor Create();
        constructor Create(V : TMalArray);
        function Rest() : TMalList;

        constructor Clone(L : TMalList);
end;

type TMalVector = class(TMalList)
end;

type TMalAtom = class(TMal)
    public
        Val: TMal;

        constructor Create(V : TMal);
end;

type TMalHashMap = class(TMal)
    public
        Val: TMalDict;
        Meta: TMal;

        constructor Create();
        constructor Create(V : TMalDict);
        constructor Create(V : TMalArray);

        constructor Clone(HM : TMalHashMap);

        function assoc_BANG(KVs: TMalArray) : TMal;
        function dissoc_BANG(Ks: TMalArray) : TMal;
end;


// General type functions

function GetBacktrace(E: Exception) : string;

function wrap_tf(x : Boolean) : TMal;

function _equal_Q(A : TMal; B : TMal) : Boolean;

function _sequential_Q(Obj: TMal) : Boolean;

function _list() : TMalList;
function _list(A: TMal) : TMalList;
function _list(A: TMal; B: TMal) : TMalList;
function _list(A: TMal; B: TMal; C: TMal) : TMalList;

function _concat(A: TMalArray; B: TMalArray) : TMalArray;

function _string_Q(Obj: TMal) : Boolean;

////////////////////////////////////////////////////////////

implementation

constructor TMalException.Create(V : TMal);
begin
    inherited Create('MalException');
    Self.Val := V;
end;

//
// Mal types
//

constructor TMalInt.Create(V : int64);
begin
    inherited Create();
    Self.Val := V;
end;

constructor TMalString.Create(V : string);
begin
    inherited Create();
    Self.Val := V;
end;

constructor TMalSymbol.Create(V : string);
begin
    inherited Create();
    Self.Val := V;
end;

constructor TMalList.Create();
begin
    inherited Create();
    SetLength(Self.Val, 0);
end;

constructor TMalList.Create(V : TMalArray);
begin
    inherited Create();
    Self.Val := V;
end;

constructor TMalList.Clone(L : TMalList);
begin
    inherited Create();
    Self.Val := copy(L.Val, 0, Length(L.Val));
end;


function TMalList.Rest() : TMalList;
begin
    if Length(Val) <= 1 then
        Rest := (_list() as TMalList)
    else
        Rest := TMalList.Create(copy(Val, 1, Length(Val)-1));
end;

// Hash Maps

constructor TMalHashMap.Create();
begin
    inherited Create();
    Self.Val := TMalDict.Create;
end;

constructor TMalHashMap.Create(V : TMalDict);
begin
    inherited Create();
    Self.Val := V;
end;

function TMalHashMap.assoc_BANG(KVs: TMalArray) : TMal;
var
    I : longint;
begin
    I := 0;
    while I < Length(KVs) do
    begin
        Self.Val[(KVs[I] as TMalString).Val] := KVs[I+1];
        I := I + 2;
    end;
    assoc_BANG := Self;
end;

function TMalHashMap.dissoc_BANG(Ks: TMalArray) : TMal;
var
    I : longint;
begin
    for I := 0 to Length(Ks)-1 do
        Self.Val.Remove((Ks[I] as TMalString).Val);
    dissoc_BANG := Self;
end;


constructor TMalHashMap.Create(V : TMalArray);
begin
    Self.Create();
    Self.assoc_BANG(V);
end;

constructor TMalHashMap.Clone(HM : TMalHashMap);
var
    I : longint;
begin
    Self.Create();
    I := 0;
    while I < HM.Val.Count do
    begin
        Self.Val[HM.Val.Keys[I]] := HM.Val[HM.Val.Keys[I]];
        I := I + 1;
    end;
end;


// Atoms

constructor TMalAtom.Create(V : TMal);
begin
    inherited Create();
    Self.Val := V;
end;

//
// General type functions
//

function GetBacktrace(E: Exception) : string;
var
  I: Integer;
  Frames: PPointer;
begin
  GetBacktrace := BackTraceStrFunc(ExceptAddr);
  Frames := ExceptFrames;
  for I := 0 to ExceptFrameCount - 1 do
    GetBacktrace := GetBacktrace + #10 + BackTraceStrFunc(Frames[I]);
end;

function wrap_tf(x : Boolean) : TMal;
begin
    if x = true then wrap_tf := TMalTrue.Create
    else             wrap_tf := TMalFalse.Create;
end;

function _equal_Q(A : TMal; B : TMal) : Boolean;
var
    I            : longint;
    ArrA, ArrB   : TMalArray;
    DictA, DictB : TMalDict;
    Key          : string;
begin
    if not ((A.ClassType = B.ClassType) or
            ((A is TMalList) and (B is TMalList))) then
        _equal_Q := false
    else
    begin
        if A is TMalList then
        begin
            ArrA := (A as TMalList).Val;
            ArrB := (B as TMalList).Val;
            if Length(ArrA) <> Length(ArrB) then
                Exit(false);
            for I := 0 to Length(ArrA)-1 do
                if not _equal_Q(ArrA[I], ArrB[I]) then
                    Exit(false);
            _equal_Q := true;
        end
        else if A is TMalHashMap then
        begin
            DictA := (A as TMalHashMap).Val;
            DictB := (B as TMalHashMap).Val;
            if DictA.Count <> DictB.Count then
                Exit(false);
            for I := 0 to DictA.Count-1 do
            begin
                Key := DictA.Keys[I];
                if DictB.IndexOf(Key) < 0 then
                    Exit(false);
                if not _equal_Q(DictA[Key], DictB[Key]) then
                    Exit(false);
            end;
            _equal_Q := true;
        end
        else if A is TMalString then
            _equal_Q := (A as TMalString).Val = (B as TMalString).Val
        else if A is TMalSymbol then
            _equal_Q := (A as TMalSymbol).Val = (B as TMalSymbol).Val
        else if A is TMalInt then
            _equal_Q := (A as TMalInt).Val = (B as TMalInt).Val
        else if A is TMalNil then
            _equal_Q := B is TMalNil
        else if A is TMalTrue then
            _equal_Q := B is TMalTrue
        else if A is TMalFalse then
            _equal_Q := B is TMalFalse
        else
            _equal_Q := A = B;
    end
end;

function _sequential_Q(Obj: TMal) : Boolean;
begin
    _sequential_Q := Obj is TMalList;
end;


function _list() : TMalList;
var
    Arr: TMalArray;
begin
    SetLength(Arr, 0);
    _list := TMalList.Create(Arr);
end;

function _list(A: TMal) : TMalList;
var
    Arr: TMalArray;
begin
    SetLength(Arr, 1);
    Arr[0] := A;
    _list := TMalList.Create(Arr);
end;

function _list(A: TMal; B: TMal) : TMalList;
var
    Arr: TMalArray;
begin
    SetLength(Arr, 2);
    Arr[0] := A;
    Arr[1] := B;
    _list := TMalList.Create(Arr);
end;

function _list(A: TMal; B: TMal; C: TMal) : TMalList;
var
    Arr: TMalArray;
begin
    SetLength(Arr, 3);
    Arr[0] := A;
    Arr[1] := B;
    Arr[2] := C;
    _list := TMalList.Create(Arr);
end;

function _concat(A: TMalArray; B: TMalArray) : TMalArray;
var
    Res : TMalArray;
    I   : longint;
begin
    SetLength(Res, Length(A) + Length(B));
    for I := 0 to Length(A)-1 do
        Res[I] := A[I];
    for I := 0 to Length(B)-1 do
        Res[I+Length(A)] := B[I];
    _concat := Res;
end;

function _string_Q(Obj: TMal) : Boolean;
var
    Str : string;
begin
    if (Obj is TMalString) then
    begin
        Str := (Obj as TMalString).Val;
        _string_Q := (Length(Str) = 0) or (Str[1] <> #127)
    end
    else
        _string_Q := false;
end;

end.
