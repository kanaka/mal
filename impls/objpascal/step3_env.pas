program Mal;

{$H+} // Use AnsiString

Uses sysutils,
     fgl,
     mal_readline,
     mal_types,
     mal_func,
     reader,
     printer,
     mal_env;

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
    A0Sym  : string;
    LetEnv : TEnv;
    Cond   : TMal;
    Fn     : TMalCallable;
    Args   : TMalArray;
    OldDict, NewDict : TMalDict;
    I                : longint;
begin
    Cond := Env.Get('DEBUG-EVAL');
    if (Cond <> nil) and not (Cond is TMalNil) and not (Cond is TMalFalse) then
       WriteLn('EVAL: ' + pr_str(Ast, True));

    if Ast is TMalSymbol then
    begin
        A0Sym := (Ast as TMalSymbol).Val;
        Cond := Env.Get(A0Sym);
        if Cond = nil then
            raise Exception.Create('''' + A0Sym+ ''' not found');
        Exit(Cond);
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
    if Arr[0] is TMalSymbol then
        A0Sym := (Arr[0] as TMalSymbol).Val
    else
        A0Sym := '__<*fn*>__';

    case A0Sym of
    'def!':
        EVAL := Env.Add((Arr[1] as TMalSymbol), EVAL(Arr[2], ENV));
    'let*':
        begin
            LetEnv := TEnv.Create(Env);
            Arr1 := (Arr[1] as TMalList).Val;
            I := 0;
            while I < Length(Arr1) do
            begin
                LetEnv.Add((Arr1[I] as TMalSymbol), EVAL(Arr1[I+1], LetEnv));
                Inc(I,2);
            end;
            EVAL := EVAL(Arr[2], LetEnv);
        end;
    else
        begin
            Cond := EVAL(Arr[0], Env);
            Args := copy(Arr, 1, Length(Arr) - 1);
            if Cond is TMalFunc then
            begin
                Fn := (Cond as TMalFunc).Val;
                for I := 0 to Length(Args) - 1 do
                    Args[I]:= EVAL(Args[I], Env);
                EVAL := Fn(Args)
            end
            else
                raise Exception.Create('invalid apply');
        end;
    end;
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
    Repl_Env.Add(TMalSymbol.Create('+'), TMalFunc.Create(@add));
    Repl_Env.Add(TMalSymbol.Create('-'), TMalFunc.Create(@subtract));
    Repl_Env.Add(TMalSymbol.Create('*'), TMalFunc.Create(@multiply));
    Repl_Env.Add(TMalSymbol.Create('/'), TMalFunc.Create(@divide));
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
