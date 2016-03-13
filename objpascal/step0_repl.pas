program Mal;

Uses CMem,
     Readline in 'pas-readline/src/readline.pas',
     History in 'pas-readline/src/history.pas';

var
    Repl_Env: string = '';
    Line : PChar;

// read
function READ(const Str: string) : string;
begin
    READ := Str;
end;

// eval
function EVAL(Ast: string; Env: string) : string;
begin
    EVAL := Ast;
end;

// print
function PRINT(Exp: string) : string;
begin
    PRINT := Exp;
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

        WriteLn(REP(Line));
    end;
end.
