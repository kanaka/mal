unit mal_readline;

{$H+} // Use AnsiString

interface

uses sysutils,
     CTypes;

{$IFDEF USE_READLINE}

{$LINKLIB readline}

{$ELSE}

{$LINKLIB libedit}

{$ENDIF}

//
function readline(Prompt: PChar) : PChar; cdecl; external;
procedure add_history(Line: PChar); cdecl; external;

type MalEOF = class(Exception);

function _readline(Prompt: string) : string;

implementation

function _readline(Prompt: string) : string;
var
    Line : PChar;
begin
    Line := readline(PChar(Prompt));
    if Line = Nil then
        raise MalEOF.Create('MalEOF');
    if Line <> '' then
        add_history(Line);

    _readline := Line;
end;

end.
