unit reader;

{$H+} // Use AnsiString

interface

Uses sysutils,
     Classes,
     RegExpr in 'regexpr/Source/RegExpr.pas',
     mal_types;

//
// Reader class
//

type TReader = class(TObject)
    public
        Tokens : TStringList;
        Position : Integer;

        constructor Create(Toks: TStringList);

        function Peek() : string;
        function Next() : string;
end;

//
// reader functions
//

function read_str(const Str: string): TMal;


implementation

//
// Reader class
//

constructor TReader.Create(Toks: TStringList);
begin
    inherited Create();
    Self.Tokens := Toks;
    Self.Position := 0;
end;

function TReader.Peek() : string;
begin
    if Position >= Tokens.Count then
        Peek := #0
    else
        Peek := Tokens[Position];
end;

function TReader.Next() : string;
begin
    Next := Tokens[Position];
    Position := Position + 1;
end;


//
// reader functions
//

function tokenize(const Str: string) : TStringList;
var
    RE       : TRegExpr;
    Tokens   : TStringList;
begin
    RE := TRegExpr.Create;
    RE.Expression := '[\s,]*(~@|[\[\]{}()''`~^@]|"(([\\].|[^\\"])*)"?|;[^\r\n]*|[^\s\[\]{}()''"`@,;]+)';
    Tokens := TStringList.Create;
    if RE.Exec(Str) then
    begin
        repeat
            if RE.Match[1][1] <> ';' then
                Tokens.Add(RE.Match[1]);
        until not RE.ExecNext;
    end;
    RE.Free;

    tokenize := Tokens;
end;


function read_atom(Reader : TReader) : TMal;
var
    RE     : TRegExpr;
    Token  : string;
    Str    : string;
begin
    RE := TRegExpr.Create;
    RE.Expression := '(^-?[0-9]+$)|(^-?[0-9][0-9.]*$)|(^nil$)|(^true$)|(^false$)|^(\".*\")$|:(.*)|(^[^\"]*$)';
    Token := Reader.Next();
    //WriteLn('token: ' + Token);
    if RE.Exec(Token) then
    begin
        if RE.Match[1] <> '' then
            read_atom := TMalInt.Create(StrToInt(RE.Match[1]))
        else if RE.Match[2] <> '' then
            // TODO
            read_atom := TMalNil.Create
        else if RE.Match[3] <> '' then
            read_atom := TMalNil.Create
        else if RE.Match[4] <> '' then
            read_atom := TMalTrue.Create
        else if RE.Match[5] <> '' then
            read_atom := TMalFalse.Create
        else if RE.Match[6] <> '' then
        begin
            Str := copy(Token, 2, Length(Token)-2);
            Str := StringReplace(Str, '\"', '"', [rfReplaceAll]);
            Str := StringReplace(Str, '\n', #10, [rfReplaceAll]);
            Str := StringReplace(Str, '\\', '\', [rfReplaceAll]);
            read_atom := TMalString.Create(Str)
        end
        else if RE.Match[7] <> '' then
            read_atom := TMalString.Create(#127 + RE.Match[7])
        else if RE.Match[8] <> '' then
            read_atom := TMalSymbol.Create(Token);
    end
    else
    begin
        RE.Free;
        raise Exception.Create('Invalid token in read_atom');
    end;
    RE.Free;
end;

// Forward declaration since read_seq calls it
function read_form(Reader : TReader) : TMal; forward;

function read_seq(Reader : TReader; start: string; last: string) : TMalArray;
var
    Token : string;
    Ast   : TMalArray;
begin
    SetLength(Ast, 0);

    Token := Reader.Next();
    if Token <> start then
        raise Exception.Create('expected ''' + start + '''');

    Token := Reader.Peek();
    while Token <> last do
    begin
        if Token = #0 then
            raise Exception.Create('expected ''' + last + ''', got EOF');
        SetLength(Ast, Length(Ast)+1);
        Ast[Length(Ast)-1] := read_form(Reader);
        Token := Reader.Peek();
    end;

    Token := Reader.Next();
    read_seq := Ast;
end;

function read_form(Reader : TReader) : TMal;
var
    Token : string;
    Meta  : TMal;
begin
    Token := Reader.Peek();
    case Token of
    // reader macros/transforms
    '''':
    begin
        Reader.Next();
        read_form := _list(TMalSymbol.Create('quote'),
                           read_form(Reader));
    end;
    '`':
    begin
        Reader.Next();
        read_form := _list(TMalSymbol.Create('quasiquote'),
                           read_form(Reader));
    end;
    '~':
    begin
        Reader.Next();
        read_form := _list(TMalSymbol.Create('unquote'),
                           read_form(Reader));
    end;
    '~@':
    begin
        Reader.Next();
        read_form := _list(TMalSymbol.Create('splice-unquote'),
                           read_form(Reader));
    end;
    '^':
    begin
        Reader.Next();
        Meta := read_form(Reader);
        read_form := _list(TMalSymbol.Create('with-meta'),
                           read_form(Reader),
                           Meta);
    end;
    '@':
    begin
        Reader.Next();
        read_form := _list(TMalSymbol.Create('deref'), read_form(Reader));
    end;

    // list
    ')': raise Exception.Create('unexpected '')''');
    '(': read_form := TMalList.Create(read_seq(Reader, '(', ')'));

    // vector
    ']': raise Exception.Create('unexpected '']''');
    '[': read_form := TMalVector.Create(read_seq(Reader, '[', ']'));

    // hash-map
    '}': raise Exception.Create('unexpected ''}''');
    '{': read_form := TMalHashMap.Create(read_seq(Reader, '{', '}'));
    else
        read_form := read_atom(Reader);
    end;
end;


function read_str(const Str: string): TMal;
var
    Tokens   : TStringList;
    //Dict     : TObjectDictionary;
begin
    Tokens := tokenize(Str);
    // TODO: check for empty list
    read_str := read_form(TReader.Create(Tokens));
end;

end.
