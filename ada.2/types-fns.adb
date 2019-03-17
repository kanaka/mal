with Ada.Unchecked_Deallocation;

with Envs;
with Err;
with Eval_Cb;
with Types.Mal;
with Types.Sequences;
with Types.Symbols;

package body Types.Fns is

   subtype AFC is Ada.Finalization.Controlled;
   use type Envs.Closure_Ptr;

   type Rec (Params_Last : Natural) is limited record
      Ast    : Mal.T;
      Refs   : Natural                                 := 1;
      Env    : Envs.Closure_Ptr                        := Envs.Null_Closure;
      Meta   : Mal.T                                   := Mal.Nil;
      Params : Symbols.Symbol_Array (1 .. Params_Last);
   end record;

   procedure Free is new Ada.Unchecked_Deallocation (Rec, Acc);
   Allocations : Natural := 0;

   ----------------------------------------------------------------------

   procedure Adjust (Object : in out Ptr) is
   begin
      Object.Ref.all.Refs := @ + 1;
   end Adjust;

   function Apply (Item : in Ptr;
                   Args : in Mal.T_Array) return Mal.T is
   begin
      pragma Assert (Item.Ref.all.Env /= Envs.Null_Closure);
      return Eval_Cb.Cb.all (Ast =>                    Item.Ref.all.Ast,
                             Env => Envs.Sub (Outer => Item.Ref.all.Env,
                                              Binds => Item.Ref.all.Params,
                                              Exprs => Args));
   end Apply;

   function Ast (Item : in Ptr) return Mal.T
   is (Item.Ref.all.Ast);

   procedure Check_Allocations is
   begin
      pragma Assert (Allocations = 0);
   end Check_Allocations;

   function Env (Item : in Ptr) return Envs.Closure_Ptr is
   begin
      pragma Assert (Item.Ref.all.Env /= Envs.Null_Closure);
      return Item.Ref.all.Env;
   end Env;

   procedure Finalize (Object : in out Ptr) is
   begin
      if Object.Ref /= null and then 0 < Object.Ref.all.Refs then
         Object.Ref.all.Refs := @ - 1;
         if 0 < Object.Ref.all.Refs then
            Object.Ref := null;
         else
            Allocations := Allocations - 1;
            Free (Object.Ref);
         end if;
      end if;
   end Finalize;

   function Params (Item : in Ptr) return Symbols.Symbol_Array
   is (Item.Ref.all.Params);

   function Meta (Item : in Ptr) return Mal.T is
   begin
      pragma Assert (Item.Ref.all.Env /= Envs.Null_Closure);
      return Item.Ref.all.Meta;
   end Meta;

   function New_Function (Params : in Sequences.Ptr;
                          Ast    : in Mal.T;
                          Env    : in Envs.Closure_Ptr)
                         return Mal.T
   is
      Ref : Acc;
   begin
      Allocations := Allocations + 1;
      --  Avoid exceptions until Ref is controlled.
      Ref := new Rec'(Params_Last => Params.Length,
                      Ast         => Ast,
                      Env         => Env,
                      others      => <>);
      return R : constant Mal.T := (Kind_Fn, (AFC with Ref)) do
         for I in 1 .. Params.Length loop
            Err.Check (Params (I).Kind = Kind_Symbol,
                       "formal parameters must be symbols");
            Ref.all.Params (I) := Params (I).Symbol;
         end loop;
      end return;
   end New_Function;

   function New_Macro (Item : in Ptr) return Mal.T is
      --  Avoid raising an exception until Ref is controlled.
      Ref : Acc := Item.Ref;
   begin
      pragma Assert (0 < Ref.all.Refs);
      if Ref.all.Refs = 1 then
         Ref.all.Refs := 2;
         Ref.all.Env := Envs.Null_Closure;
         --  Finalize the environment, it will not be used anymore.
         Ref.all.Meta := Mal.Nil;
      else
         Allocations := Allocations + 1;
         Ref := new Rec'(Params_Last => Ref.all.Params_Last,
                         Params      => Ref.all.Params,
                         Ast         => Ref.all.Ast,
                         others      => <>);
      end if;
      return (Kind_Macro, (AFC with Ref));
   end New_Macro;

   function With_Meta (Item     : in Ptr;
                       Metadata : in Mal.T) return Mal.T
   is
      --  Avoid raising an exception until Ref is controlled.
      Ref : Acc := Item.Ref;
   begin
      pragma Assert (Ref.all.Env /= Envs.Null_Closure);
      pragma Assert (0 < Ref.all.Refs);
      if Ref.all.Refs = 1 then
         Ref.all.Refs := 2;
         Ref.all.Meta := Metadata;
      else
         Allocations := Allocations + 1;
         Ref := new Rec'(Params_Last => Ref.all.Params_Last,
                         Params      => Ref.all.Params,
                         Ast         => Ref.all.Ast,
                         Env         => Ref.all.Env,
                         Meta        => Metadata,
                         others      => <>);
      end if;
      return (Kind_Fn, (AFC with Ref));
   end With_Meta;

end Types.Fns;
