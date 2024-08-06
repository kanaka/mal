program Mal;

{$H+} // Use AnsiString

Uses sysutils,
     fgl,
     mal_readline,
     mal_types,
     mal_func,
     reader,
     printer;

type
    TEnv = specialize TFPGMap<string,TMal>;

var
    Repl_Env : TEnv;
    Line     : string;

// read
function READ(const Str: string) : TMal;
begin
    READ := read_str(Str);
end;

// eval
function EVAL(Ast: TMal; Env: TEnv) : TMal;
var
    Arr    : TMalArray;
    Arr1   : TMalArray;
    Sym              : string;
    Cond   : TMal;
    Fn     : TMalFunc;
    Args   : TMalArray;
    OldDict, NewDict : TMalDict;
    I                : longint;
begin
    // WriteLn('EVAL: ' + pr_str(Ast, True));

    if Ast is TMalSymbol then
    begin
        Sym := (Ast as TMalSymbol).Val;
        if Env.IndexOf(Sym) < 0 then
            raise Exception.Create('''' + Sym + ''' not found')
        else
            Exit(Env[Sym]);
    end
    else if Ast is TMalVector then
    begin
        Arr := (Ast as TMalVector).Val;
        SetLength(Arr1, Length(Arr));
        for I := 0 to Length(Arr)-1 do
            Arr1[I]:= EVAL(Arr[I], Env);
        Exit(TMalVector.Create(Arr1));
    end
    else if Ast is TMalHashMap then
    begin
        OldDict := (Ast as TMalHashMap).Val;
        NewDict := TMalDict.Create;
        for I := 0 to OldDict.Count-1 do
            NewDict[OldDict.Keys[I]]:= EVAL(OldDict[OldDict.Keys[I]], Env);
        Exit(TMalHashMap.Create(NewDict));
    end
    else if not (Ast is TMalList) then
        Exit(Ast);

    // Apply list
    Arr := (Ast as TMalList).Val;
    if Length(Arr) = 0 then
        Exit(Ast);
    Cond := EVAL(Arr[0], Env);
    Args := copy(Arr, 1, Length(Arr) - 1);
    if Cond is TMalFunc then
    begin
        Fn := (Cond as TMalFunc);
        for I := 0 to Length(Args) - 1 do
            Args[I]:= EVAL(Args[I], Env);
        EVAL := Fn.Val(Args)
    end
    else
        raise Exception.Create('invalid apply');
end;

// print
function PRINT(Exp: TMal) : string;
begin
    PRINT := pr_str(Exp, True);
end;

// repl
function REP(Str: string) : string;
begin
    REP := PRINT(EVAL(READ(Str), Repl_Env));
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

begin
    Repl_Env := TEnv.Create;
    Repl_Env.Add('+', TMalFunc.Create(@add));
    Repl_Env.Add('-', TMalFunc.Create(@subtract));
    Repl_Env.Add('*', TMalFunc.Create(@multiply));
    Repl_Env.Add('/', TMalFunc.Create(@divide));
    while True do
    begin
        try
            Line := _readline('user> ');
            if Line = '' then continue;
            WriteLn(REP(Line))
        except
            On E : MalEOF do Halt(0);
            On E : Exception do
            begin
                WriteLn('Error: ' + E.message);
                WriteLn('Backtrace:');
                WriteLn(GetBacktrace(E));
            end;
        end;
    end;
end.
