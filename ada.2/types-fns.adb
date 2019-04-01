with Err;
with Eval_Cb;

package body Types.Fns is

   use type Envs.Ptr;

   ----------------------------------------------------------------------

   function Apply (Item : in Instance;
                   Args : in Mal.T_Array) return Mal.T
   is (Eval_Cb.Cb.all (Ast => Item.F_Ast,
                       Env => Envs.New_Env (Outer => Item.F_Env,
                                            Binds => Item.F_Params,
                                            Exprs => Args)));

   function Ast (Item : in Instance) return Mal.T
   is (Item.F_Ast);

   function Env (Item : in Instance) return Envs.Ptr
   is (Item.F_Env);

   procedure Keep_References (Object : in out Instance) is
   begin
      Mal.Keep (Object.F_Ast);
      if Object.F_Env /= null then
         Object.F_Env.all.Keep;
      end if;
      Mal.Keep (Object.F_Meta);
   end Keep_References;

   function Meta (Item : in Instance) return Mal.T
   is (Item.F_Meta);

   function New_Function (Params : in Sequences.Instance;
                          Ast    : in Mal.T;
                          Env    : in Envs.Ptr)
                         return Mal.T
   is
      Ref : constant Mal.Fn_Ptr
        := new Instance'(Garbage_Collected.Instance with
                         Last   => Params.Length,
                         F_Ast  => Ast,
                         F_Env  => Env,
                         others => <>);
   begin
      Garbage_Collected.Register (Garbage_Collected.Pointer (Ref));
      for I in Ref.all.F_Params'Range loop
         Err.Check (Params (I).Kind = Kind_Symbol,
                    "formal parameters must be symbols");
         Ref.all.F_Params (I) := Params (I).Symbol;
      end loop;
      return (Kind_Fn, Ref);
   end New_Function;

   function New_Macro (Item : in Instance) return Mal.T is
      Ref : constant Mal.Fn_Ptr
        := new Instance'(Garbage_Collected.Instance with
                         Last     => Item.Last,
                         F_Params => Item.F_Params,
                         F_Ast    => Item.F_Ast,
                         others   => <>);
   begin
      Garbage_Collected.Register (Garbage_Collected.Pointer (Ref));
      return (Kind_Macro, Ref);
   end New_Macro;

   function Params (Item : in Instance) return Symbols.Symbol_Array
   is (Item.F_Params);

   function With_Meta (Item     : in Instance;
                       Metadata : in Mal.T) return Mal.T
   is
      Ref : constant Mal.Fn_Ptr
        := new Instance'(Garbage_Collected.Instance with
                         Last     => Item.Last,
                         F_Params => Item.F_Params,
                         F_Ast    => Item.F_Ast,
                         F_Env    => Item.F_Env,
                         F_Meta   => Metadata);
   begin
      Garbage_Collected.Register (Garbage_Collected.Pointer (Ref));
      return (Kind_Fn, Ref);
   end With_Meta;

end Types.Fns;
