unit mal_func;

interface

uses mal_types,
     mal_env;

// Some general type definitions

type
    TMalCallable = function (Args : TMalArray) : TMal;

type TMalFunc = class(TMal)
    public
        Val     : TMalCallable;
        Ast     : TMal;
        Env     : TEnv;
        Params  : TMalList;
        isMacro : Boolean;
        Meta    : TMal;

        constructor Create(V : TMalCallable);
        constructor Create(A  : TMal;
                           E  : TEnv;
                           P  : TMalList);

        constructor Clone(F : TMalFunc);
end;

////////////////////////////////////////////////////////////

implementation

constructor TMalFunc.Create(V : TMalCallable);
begin
    inherited Create();
    Self.Val := V;
end;

constructor TMalFunc.Create(A  : TMal;
                            E  : TEnv;
                            P  : TMalList);
begin
    inherited Create();
    Self.Ast    := A;
    Self.Env    := E;
    Self.Params := P;
end;

constructor TMalFunc.Clone(F : TMalFunc);
begin
    Self.Create(F.Ast, F.Env, F.Params);
    Self.isMacro := F.isMacro;
    Self.Meta := F.Meta;
end;

end.
