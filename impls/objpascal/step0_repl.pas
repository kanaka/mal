program Mal;

{$H+} // Use AnsiString

Uses CMem,
     mal_readline;

var
    Repl_Env: string = '';
    Line : string;

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
        try
            Line := _readline('user> ');
            if Line = '' then continue;
            WriteLn(REP(Line))
        except
            On E : MalEOF do Halt(0);
        end;
    end;
end.
