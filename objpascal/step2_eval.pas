program Mal;

{$H+} // Use AnsiString

Uses sysutils,
     CMem,
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
// Forward declation since eval_ast call it
function EVAL(Ast: TMal; Env: TEnv) : TMal; forward;

function eval_ast(Ast: TMal; Env: TEnv) : TMal;
var
    Sym              : string;
    OldArr, NewArr   : TMalArray;
    OldDict, NewDict : TMalDict;
    I                : longint;
begin
    if Ast is TMalSymbol then
    begin
        Sym := (Ast as TMalSymbol).Val;
        if Env.IndexOf(Sym) < 0 then
            raise Exception.Create('''' + Sym + ''' not found')
        else
            eval_ast := Env[Sym];
    end
    else if Ast is TMalList then
    begin
        OldArr := (Ast as TMalList).Val;
        SetLength(NewArr, Length(OldArr));
        for I := 0 to Length(OldArr)-1 do
        begin
            NewArr[I] := EVAL(OldArr[I], Env);
        end;
        if Ast is TMalVector then
            eval_ast := TMalVector.Create(NewArr)
        else
            eval_ast := TMalList.Create(NewArr);
    end
    else if Ast is TMalHashMap then
    begin
        OldDict := (Ast as TMalHashMap).Val;
        NewDict := TMalDict.Create;
        I := 0;
        while I < OldDict.Count do
        begin
            NewDict[OldDict.Keys[I]] := EVAL(OldDict[OldDict.Keys[I]], Env);
            I := I + 1;
        end;
        eval_ast := TMalHashMap.Create(NewDict);
    end
    else
        eval_ast := Ast;
end;

function EVAL(Ast: TMal; Env: TEnv) : TMal;
var
    Arr    : TMalArray;
    Fn     : TMalCallable;
begin
    if Ast.ClassType <> TMalList then
        Exit(eval_ast(Ast, Env));

    // Apply list
    Arr := (eval_ast(Ast, Env) as TMalList).Val;
    if Arr[0] is TMalFunc then
    begin
        Fn := (Arr[0] as TMalFunc).Val;
        EVAL := Fn(copy(Arr, 1, Length(Arr)-1));
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
