unit printer;

{$H+} // Use AnsiString

interface

Uses sysutils,
     mal_types,
     mal_func;

function pr_str_array(Args : TMalArray;
                      print_readably : Boolean;
                      Separator : string) : string;

function pr_str(Obj : TMal; print_readably : Boolean) : string;

implementation

function pr_str_array(Args : TMalArray;
                      print_readably : Boolean;
                      Separator : string) : string;
var
    Str : string;
    I   : longint;
begin
    Str := '';
    for I := 0 to Length(Args)-1 do
    begin
        Str := Str + pr_str(Args[I], print_readably);
        if I <> Length(Args)-1 then
            Str := Str + Separator;
    end;
    pr_str_array := Str;
end;

function pr_str_dict(Dict : TMalDict;
                     print_readably : Boolean;
                     Separator : string) : string;
var
    I    : longint;
    Arr  : TMalArray;
begin
    SetLength(Arr, Dict.Count * 2);
    I := 0;
    while I < Dict.Count do
    begin
        Arr[I*2]   := TMalString.Create(Dict.Keys[I]);
        Arr[I*2+1] := Dict[Dict.Keys[I]];
        I := I + 1;
    end;
    pr_str_dict := pr_str_array(Arr, print_readably, ' ');
end;


function pr_str(Obj : TMal; print_readably : Boolean) : string;
var
    Str  : string;
    Fn   : TMalFunc;
begin
    if Obj.ClassType = TMalList then
        pr_str := '(' + pr_str_array((Obj as TMalList).Val,
                                     print_readably,
                                     ' ') + ')'
    else if Obj.ClassType = TMalVector then
        pr_str := '[' + pr_str_array((Obj as TMalList).Val,
                                     print_readably,
                                     ' ') + ']'
    else if Obj is TMalHashMap then
        pr_str := '{' + pr_str_dict((Obj as TMalHashMap).Val,
                                     print_readably,
                                     ' ') + '}'
    else if Obj is TMalString then
    begin
        Str := (Obj as TMalString).Val;
        if (Length(Str) > 0) and (Str[1] = #127) then
            pr_str := ':' + copy(Str, 2, Length(Str))
        else if print_readably then
        begin
            Str := StringReplace(Str, '\', '\\', [rfReplaceAll]);
            Str := StringReplace(Str, '"', '\"', [rfReplaceAll]);
            Str := StringReplace(Str, #10, '\n', [rfReplaceAll]);
            pr_str := Format('"%s"', [Str])
        end
        else
            pr_str := Str;
    end
    else if Obj is TMalNil then
        pr_str := 'nil'
    else if Obj is TMalTrue then
        pr_str := 'true'
    else if Obj is TMalFalse then
        pr_str := 'false'
    else if Obj is TMalInt then
        pr_str := IntToStr((Obj as TMalInt).Val)
    else if Obj is TMalSymbol then
        pr_str := (Obj as TMalSymbol).Val
    else if Obj is TMalAtom then
        pr_str := '(atom ' +
                  pr_str((Obj as TMalAtom).Val, print_readably) +
                  ')'
    else if Obj is TMalFunc then
    begin
        Fn := (Obj as TMalFunc);
        if Fn.Ast = nil then
            pr_str := '#<native function>'
        else
            pr_str := '(fn* ' + pr_str(Fn.Params,true) +
                      ' ' + pr_str(Fn.Ast,true) + ')'
    end
    else
        pr_str := '#unknown';
end;

end.
