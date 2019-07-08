with Envs;
with Garbage_Collected;

package Types.Fns is

   Eval_Cb : access function (Ast : in T;
                              Env : in Envs.Ptr) return T;
   --  The main program must register this global callback to the main
   --  eval function before Apply is called.

   type Instance (<>) is abstract new Garbage_Collected.Instance with private;

   function New_Function (Params   : in Sequence_Ptr;
                          Ast      : in T;
                          Env      : in Envs.Ptr;
                          Metadata : in T            := Nil) return Fn_Ptr
     with Inline;
   --  Raise an exception if Params contains something else than symbols.

   function Params (Item : in Instance) return Sequence_Ptr
     with Inline;
   function Ast (Item : in Instance) return T with Inline;
   --  Useful to print.

   function Apply (Item : in Instance;
                   Args : in T_Array) return T with Inline;
   --  Duplicated in the step files because of TCO.

   function Env (Item : in Instance) return Envs.Ptr with Inline;
   --  Required for TCO, instead of Apply.

   function Meta (Item : in Instance) return T with Inline;

private

   type Instance is new Garbage_Collected.Instance
     with record
        F_Ast    : T;
        F_Env    : Envs.Ptr;
        F_Meta   : T;
        F_Params : Sequence_Ptr;
   end record;
   overriding procedure Keep_References (Object : in out Instance) with Inline;

end Types.Fns;
