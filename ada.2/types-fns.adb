with Err;
pragma Warnings (Off, "unit ""Types.Sequences"" is not referenced");
with Types.Sequences;
pragma Warnings (On, "unit ""Types.Sequences"" is not referenced");

package body Types.Fns is

   function Apply (Item : in Instance;
                   Args : in T_Array) return T
   is
      Env : constant Envs.Ptr := Envs.New_Env (Outer => Item.F_Env);
   begin
      Env.all.Set_Binds (Binds => Item.F_Params.all.Data,
                         Exprs => Args);
      return Eval_Cb.all (Ast => Item.F_Ast,
                          Env => Env);
   end Apply;

   function Ast (Item : in Instance) return T
   is (Item.F_Ast);

   function Env (Item : in Instance) return Envs.Ptr
   is (Item.F_Env);

   procedure Keep_References (Object : in out Instance) is
   begin
      Keep (Object.F_Ast);
      Object.F_Params.all.Keep;
      Object.F_Env.all.Keep;
      Keep (Object.F_Meta);
   end Keep_References;

   function Meta (Item : in Instance) return T
   is (Item.F_Meta);

   function New_Function (Params   : in Sequence_Ptr;
                          Ast      : in T;
                          Env      : in Envs.Ptr;
                          Metadata : in T            := Nil) return Fn_Ptr
   is
      --  Env and Params are not null and require an immediate
      --  initialization.
      Ref : constant Fn_Ptr
        := new Instance'(Garbage_Collected.Instance with
                         F_Ast    => Ast,
                         F_Env    => Env,
                         F_Meta   => Metadata,
                         F_Params => Params);
   begin
      Garbage_Collected.Register (Garbage_Collected.Pointer (Ref));
      Err.Check ((for all P of Params.all.Data => P.Kind = Kind_Symbol),
                 "formal parameters must be symbols");
      return Ref;
   end New_Function;

   function Params (Item : in Instance) return Sequence_Ptr
   is (Item.F_Params);

end Types.Fns;
