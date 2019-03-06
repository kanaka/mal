with Ada.Unchecked_Deallocation;

with Envs;
with Eval_Cb;
with Types.Lists;
with Types.Mal;
with Types.Symbols;

package body Types.Functions is

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

   ----------------------------------------------------------------------

   procedure Adjust (Object : in out Ptr) is
   begin
      Object.Ref.all.Refs := Object.Ref.all.Refs + 1;
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

   function Env (Item : in Ptr) return Envs.Closure_Ptr is
   begin
      pragma Assert (Item.Ref.all.Env /= Envs.Null_Closure);
      return Item.Ref.all.Env;
   end Env;

   procedure Finalize (Object : in out Ptr) is
   begin
      if Object.Ref /= null and then 0 < Object.Ref.all.Refs then
         Object.Ref.all.Refs := Object.Ref.all.Refs - 1;
         if 0 < Object.Ref.all.Refs then
            Object.Ref := null;
         else
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

   function New_Function (Params : in Lists.Ptr;
                          Ast    : in Mal.T;
                          Env    : in Envs.Closure_Ptr)
                         return Mal.T
   is
      Ref : constant Acc := new Rec'(Params_Last => Params.Length,
                                     Ast         => Ast,
                                     Env         => Env,
                                     others      => <>);
   begin
      for I in 1 .. Params.Length loop
         Ref.all.Params (I) := Params.Element (I).Symbol;
      end loop;
      return (Kind_Function, (AFC with Ref));
   end New_Function;

   function New_Macro (Item : in Ptr) return Mal.T is
      Old : Rec renames Item.Ref.all;
      Ref : Acc;
   begin
      pragma Assert (0 < Old.Refs);
      if Old.Refs = 1 then
         Ref := Item.Ref;
         Old.Refs := 2;
         Old.Env := Envs.Null_Closure;
         --  Finalize the environment, it will not be used anymore.
         Old.Meta := Mal.Nil;
      else
         Ref := new Rec'(Params_Last => Old.Params_Last,
                         Params      => Old.Params,
                         Ast         => Old.Ast,
                         others      => <>);
      end if;
      return (Kind_Macro, (AFC with Ref));
   end New_Macro;

   function With_Meta (Item     : in Ptr;
                       Metadata : in Mal.T) return Mal.T
   is
      Old : Rec renames Item.Ref.all;
      Ref : Acc;
   begin
      pragma Assert (Old.Env /= Envs.Null_Closure);
      pragma Assert (0 < Old.Refs);
      if Old.Refs = 1 then
         Ref := Item.Ref;
         Old.Refs := 2;
         Old.Meta := Metadata;
      else
         Ref := new Rec'(Params_Last => Old.Params_Last,
                         Params      => Old.Params,
                         Ast         => Old.Ast,
                         Env         => Old.Env,
                         Meta        => Metadata,
                         others      => <>);
      end if;
      return (Kind_Function, (AFC with Ref));
   end With_Meta;

end Types.Functions;
