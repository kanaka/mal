program Mal;

Uses sysutils,
     CMem,
     Readline in 'pas-readline/src/readline.pas',
     History in 'pas-readline/src/history.pas',
     mal_types,
     reader,
     printer;

var
    Repl_Env: string = '';
    Line : PChar;

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
        Line := Readline.readline('user> ');
        if Line = Nil then
            Halt(0);
        if Line[0] = #0 then
            continue;
        add_history(Line);

        try
            WriteLn(REP(Line))
        except
            On E : Exception do
            begin
                WriteLn('Error: ' + E.message);
                WriteLn('Backtrace:');
                WriteLn(GetBacktrace(E));
            end;
        end;
    end;
end.
