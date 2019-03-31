with Envs;
with Garbage_Collected;
with Types.Mal;
with Types.Sequences;
with Types.Symbols;

package Types.Fns is

   type Instance (<>) is new Garbage_Collected.Instance with private;
   --  A pointer to an user-defined function or macro.

   function New_Function (Params : in Types.Sequences.Instance;
                          Ast    : in Mal.T;
                          Env    : in Envs.Ptr) return Mal.T
     with Inline;
   --  Raise an exception if Params contains something else than symbols.

   function New_Macro (Item : in Instance) return Mal.T with Inline;

   function Params (Item : in Instance) return Symbols.Symbol_Array
     with Inline;
   function Ast (Item : in Instance) return Mal.T with Inline;
   --  Useful to print.

   function Apply (Item : in Instance;
                   Args : in Mal.T_Array) return Mal.T with Inline;
   --  Returns null for macros.

   function Env (Item : in Instance) return Envs.Ptr with Inline;
   --  Returns null for macros.
   --  Required for TCO, instead of Apply.

   function Meta (Item : in Instance) return Mal.T with Inline;
   function With_Meta (Item     : in Instance;
                       Metadata : in Mal.T) return Mal.T with Inline;

private

   type Instance (Last : Natural) is new Garbage_Collected.Instance
     with record
        F_Ast    : Mal.T;
        F_Env    : Envs.Ptr;
        F_Meta   : Mal.T;
        F_Params : Symbols.Symbol_Array (1 .. Last);
   end record;
   overriding procedure Keep_References (Object : in out Instance) with Inline;

end Types.Fns;
