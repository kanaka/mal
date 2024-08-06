program Mal;

{$H+} // Use AnsiString

Uses sysutils,
     fgl,
     mal_readline,
     mal_types,
     mal_func,
     reader,
     printer,
     mal_env,
     core;

var
    Repl_Env : TEnv;
    Line     : string;
    I        : longint;
    Key      : string;

// read
function READ(const Str: string) : TMal;
begin
    READ := read_str(Str);
end;

// eval
function EVAL(Ast: TMal; Env: TEnv) : TMal;
var
    Lst    : TMalList;
    Arr    : TMalArray;
    Arr1   : TMalArray;
    A0Sym  : string;
    LetEnv : TEnv;
    FnEnv  : TEnv;
    Cond   : TMal;
    I      : longint;
    Fn     : TMalFunc;
    Args   : TMalArray;
    OldDict, NewDict :  TMalDict;
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
    Lst := (Ast as TMalList);
    Arr := Lst.Val;
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
    'do':
        begin
            for I := 1 to Length(Arr) - 2 do
                 Cond := EVAL(Arr[I], Env);
           EVAL := EVAL(Arr[Length(Arr)-1], Env);
        end;
    'if':
        begin
            Cond := EVAL(Arr[1], Env);
            if (Cond is TMalNil) or (Cond is TMalFalse) then
                if Length(Arr) > 3 then
                    EVAL := EVAL(Arr[3], Env)
                else
                    EVAL := TMalNil.Create
            else
                EVAL := EVAL(Arr[2], Env);
        end;
    'fn*':
        begin
            EVAL := TMalFunc.Create(Arr[2], Env, (Arr[1] as TMalList))
        end;
    else
        begin
            Cond := EVAL(Arr[0], Env);
            Args := copy(Arr, 1, Length(Arr) - 1);
            if Cond is TMalFunc then
            begin
                Fn := Cond as TMalFunc;
                for I := 0 to Length(Args) - 1 do
                    Args[I]:= EVAL(Args[I], Env);
                if Fn.Ast = nil then
                    EVAL := Fn.Val(Args)
                else
                begin
                    FnEnv := TEnv.Create(Fn.Env, Fn.Params, Args);
                    EVAL := EVAL(Fn.Ast, FnEnv);
                end

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

begin
    Repl_Env := TEnv.Create;

    // core.pas: defined using Pascal
    for I := 0 to core.NS.Count-1 do
    begin
        Key := core.NS.Keys[I];
        Repl_Env.Add(TMalSymbol.Create(Key),
                     TMalFunc.Create(core.NS[Key]));
    end;

    // core.mal: defined using language itself
    REP('(def! not (fn* (a) (if a false true)))');

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
