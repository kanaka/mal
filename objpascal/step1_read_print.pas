program Mal;

{$H+} // Use AnsiString

Uses sysutils,
     CMem,
     mal_readline,
     mal_types,
     reader,
     printer;

var
    Repl_Env : string = '';
    Line     : string;

// read
function READ(const Str: string) : TMal;
begin
    READ := read_str(Str);
end;

// eval
function EVAL(Ast: TMal; Env: string) : TMal;
begin
    EVAL := Ast;
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
