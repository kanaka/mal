unit mal_env;

{$H+} // Use AnsiString

interface

Uses sysutils,
     fgl,
     mal_types;

type TEnv = class(TObject)
    public
        Data  : TMalDict;
        Outer : TEnv;

        constructor Create;
        constructor Create(_Outer : TEnv);
        constructor Create(_Outer : TEnv;
                           Binds  : TMalList;
                           Exprs  : TMalArray);

        function Add(Key : TMalSymbol; Val : TMal) : TMal;
        function Get(Key : String) : TMal;
end;

////////////////////////////////////////////////////////////

implementation

constructor TEnv.Create();
begin
    inherited Create();
    Self.Data  := TMalDict.Create;
    Self.Outer := nil;
end;

constructor TEnv.Create(_Outer: TEnv);
begin
    Self.Create();
    Self.Outer := _Outer;
end;

constructor TEnv.Create(_Outer : TEnv;
                        Binds  : TMalList;
                        Exprs  : TMalArray);
var
    I     : longint;
    Bind  : TMalSymbol;
    Rest  : TMalList;
begin
    Self.Create(_Outer);
    for I := 0 to Length(Binds.Val)-1 do
    begin
        Bind := (Binds.Val[I] as TMalSymbol);
        if Bind.Val = '&' then
        begin
            if I < Length(Exprs) then
                Rest := TMalList.Create(copy(Exprs, I, Length(Exprs)-I))
            else
                Rest := TMalList.Create;
            Self.Data[(Binds.Val[I+1] as TMalSymbol).Val] := Rest;
            break;
        end;
        Self.Data[Bind.Val] := Exprs[I];
    end;
end;

function TEnv.Add(Key : TMalSymbol; Val : TMal) : TMal;
begin
    Self.Data[Key.Val] := Val;
    Add := Val;
end;

function TEnv.Get(Key : String) : TMal;
begin
    if Data.IndexOf(Key) >= 0 then
        Get := Data[Key]
    else if Outer <> nil then
        Get := Outer.Get(Key)
    else
        Get := nil;
end;

end.
