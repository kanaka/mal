program Mal;

{$H+} // Use AnsiString

Uses sysutils,
     fgl,
     math,
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
    CmdArgs  : TMalArray;

// read
function READ(const Str: string) : TMal;
begin
    READ := read_str(Str);
end;

// eval

function starts_with(Ast: TMal; Sym: String) : Boolean;
var
   Arr : TMalArray;
   A0  : TMal;
begin
   if Ast.ClassType <> TMalList then Exit (False);
   Arr := (Ast as TMalList).Val;
   if Length (Arr) = 0 then Exit (False);
   A0 := Arr [0];
   starts_with := (A0.ClassType = TMalSymbol) and ((A0 as TMalSymbol).Val = Sym);
end;

function quasiquote(Ast: TMal) : TMal;
var
    Arr      : TMalArray;
    Res, Elt : TMal;
    I        : longint;
begin
    if Ast is TMalSymbol or Ast is TMalHashMap then
        Exit(_list(TMalSymbol.Create('quote'), Ast));

    if not (Ast is TMalList) then
        Exit(Ast);

    Arr := (Ast as TMalList).Val;
    if starts_with (Ast, 'unquote') then Exit(Arr[1]);

    Res := _list();
    for I := 1 to Length(Arr) do
    begin
        Elt := Arr [Length(Arr) - I];
        if starts_with (Elt, 'splice-unquote') then
            Res := _list(TMalSymbol.Create('concat'), (Elt as TMalList).Val[1], Res)
        else
            Res := _list(TMalSymbol.Create('cons'), quasiquote (Elt), Res);
    end;
    if Ast.ClassType <> TMalList then
        Exit(_list(TMalSymbol.Create('vec'), Res))
    else
        Exit(Res);
end;

function EVAL(Ast: TMal; Env: TEnv) : TMal;
var
    Lst    : TMalList;
    Arr    : TMalArray;
    Arr1   : TMalArray;
    A0Sym  : string;
    LetEnv : TEnv;
    Cond   : TMal;
    I      : longint;
    Fn     : TMalFunc;
    Args   : TMalArray;
    Err    : TMalArray;
    OldDict, NewDict :  TMalDict;
begin
  while true do
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
        Exit(Env.Add((Arr[1] as TMalSymbol), EVAL(Arr[2], ENV)));
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
            Env := LetEnv;
            Ast := Arr[2]; // TCO
        end;
    'quote':
        Exit(Arr[1]);
    'quasiquote':
        Ast := quasiquote(Arr[1]);
    'defmacro!':
    begin
        Fn := EVAL(Arr[2], ENV) as TMalFunc;
        Fn := TMalFunc.Clone(Fn);
        Fn.isMacro := true;
        Exit(Env.Add((Arr[1] as TMalSymbol), Fn));
    end;
    'try*':
    begin
        try
            Exit(EVAL(Arr[1], Env));
        except
            On E : Exception do
            begin
                if Length(Arr) < 3 then
                    raise;
                SetLength(Err, 1);
                if E.ClassType = TMalException then
                    Err[0] := (E as TMalException).Val
                else
                    Err[0] := TMalString.Create(E.message);
                Arr := (Arr[2] as TMalList).Val;
                Exit(EVAL(Arr[2], TEnv.Create(Env,
                                              _list(Arr[1]),
                                              Err)));
            end;
        end;
    end;
    'do':
        begin
            for I := 1 to Length(Arr) - 2 do
                 Cond := EVAL(Arr[I], Env);
            Ast := Arr[Length(Arr)-1]; // TCO
        end;
    'if':
        begin
            Cond := EVAL(Arr[1], Env);
            if (Cond is TMalNil) or (Cond is TMalFalse) then
                if Length(Arr) > 3 then
                    Ast := Arr[3] // TCO
                else
                    Exit(TMalNil.Create)
            else
                Ast := Arr[2]; // TCO
        end;
    'fn*':
        begin
            Exit(TMalFunc.Create(Arr[2], Env, (Arr[1] as TMalList)));
        end;
    else
        begin
            Cond := EVAL(Arr[0], Env);
            Args := copy(Arr, 1, Length(Arr) - 1);
            if Cond is TMalFunc then
            begin
                Fn := Cond as TMalFunc;
                if Fn.isMacro then
                begin
                    if Fn.Ast =nil then
                        Ast := Fn.Val(Args)
                    else
                        Ast := EVAL(Fn.Ast, Tenv.Create(Fn.Env, Fn.Params, Args));
                    continue; // TCO
                end;
                for I := 0 to Length(Args) - 1 do
                    Args[I]:= EVAL(Args[I], Env);
                if Fn.Ast = nil then
                    Exit(Fn.Val(Args))
                else
                begin
                    Env := TEnv.Create(Fn.Env, Fn.Params, Args);
                    Ast := Fn.Ast; // TCO
                end

            end
            else
                raise Exception.Create('invalid apply');
        end;
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

function do_eval(Args : TMalArray) : TMal;
begin
    do_eval := EVAL(Args[0], Repl_Env);
end;

begin
    Repl_Env := TEnv.Create;
    core.EVAL := @EVAL;

    // core.pas: defined using Pascal
    for I := 0 to core.NS.Count-1 do
    begin
        Key := core.NS.Keys[I];
        Repl_Env.Add(TMalSymbol.Create(Key),
                     TMalFunc.Create(core.NS[Key]));
    end;
    Repl_Env.Add(TMalSymbol.Create('eval'), TMalFunc.Create(@do_eval));
    SetLength(CmdArgs, Max(0, ParamCount-1));
    for I := 2 to ParamCount do
        CmdArgs[I-2] := TMalString.Create(ParamStr(I));
    Repl_Env.Add(TMalSymbol.Create('*ARGV*'), TMalList.Create(CmdArgs));
    Repl_Env.Add(TMalSymbol.Create('*host-language*'),
                 TMalString.Create('Object Pascal'));

    // core.mal: defined using language itself
    REP('(def! not (fn* (a) (if a false true)))');
    REP('(def! load-file (fn* (f) (eval (read-string (str "(do " (slurp f) "\nnil)")))))');
    REP('(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list ''if (first xs) (if (> (count xs) 1) (nth xs 1) (throw "odd number of forms to cond")) (cons ''cond (rest (rest xs)))))))');


    if ParamCount >= 1 then
    begin
        REP('(load-file "' + ParamStr(1) + '")');
        ExitCode := 0;
        Exit;
    end;

    REP('(println (str "Mal [" *host-language* "]"))');
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
                if E.ClassType = TMalException then
                    WriteLn('Error: ' + pr_str((E as TMalException).Val, True))
                else
                    WriteLn('Error: ' + E.message);
                WriteLn('Backtrace:');
                WriteLn(GetBacktrace(E));
            end;
        end;
    end;
end.
