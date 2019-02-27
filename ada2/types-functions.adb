with Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;

with Environments;
with Printer;
with Types.Lists;
with Types.Mal;
with Types.Symbols.Names;

package body Types.Functions is

   subtype AFC is Ada.Finalization.Controlled;
   package ASU renames Ada.Strings.Unbounded;
   use type Types.Symbols.Ptr;

   type Rec is limited record
      Refs    : Natural                  := 1;
      Args    : Lists.Ptr;
      Expr    : Mal.T;
      Env     : Environments.Closure_Ptr;
      Varargs : Boolean;
      Meta    : Mal.T                    := Mal.Nil;
   end record;

   procedure Free is new Ada.Unchecked_Deallocation (Rec, Acc);

   ----------------------------------------------------------------------

   procedure Adjust (Object : in out Ptr) is
   begin
      Object.Ref.all.Refs := Object.Ref.all.Refs + 1;
   end Adjust;

   function Closure (Item : in Ptr) return Environments.Closure_Ptr
   is (Item.Ref.all.Env);

   function Expression (Item : in Ptr) return Mal.T
   is (Item.Ref.all.Expr);

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

   function Formals (Item : in Ptr) return Lists.Ptr
   is (Item.Ref.all.Args);

   function Meta (Item : in Ptr) return Mal.T
   is (Item.Ref.all.Meta);

   function New_Function (Formals     : in Lists.Ptr;
                          Expression  : in Mal.T;
                          Environment : in Environments.Closure_Ptr)
                         return Mal.T
   is (Kind_Function,
       (AFC with new Rec'
          (Args    => Formals,
           Expr    => Expression,
           Env     => Environment,
           Varargs => 1 < Formals.Length
                      and then Formals.Element (Formals.Length - 1).Symbol
                               = Symbols.Names.Ampersand,
           others  => <>)));

   function New_Macro (Item : in Ptr) return Mal.T is
      Old : Rec renames Item.Ref.all;
      Ref : Acc;
   begin
      pragma Assert (0 < Old.Refs);
      if Old.Refs = 1 then
         Ref := Item.Ref;
         Old.Refs := 2;
         Old.Env := Environments.Null_Closure;
         --  Finalize the previous closure.
         Old.Meta := Mal.Nil;
      else
         Ref := new Rec'(Args    => Item.Ref.all.Args,
                         Expr    => Item.Ref.all.Expr,
                         Varargs => Item.Ref.all.Varargs,
                         others  => <>);
      end if;
      return (Kind_Macro, (AFC with Ref));
   end New_Macro;

   procedure Set_Binds (Item : in Ptr;
                        Env  : in Environments.Ptr;
                        Args : in Mal.T_Array) is
      R : Rec renames Item.Ref.all;
   begin
      if R.Varargs then
         if Args'Length < R.Args.Length - 2 then
            raise Argument_Error with "expected "
              & ASU.To_String (Printer.Pr_Str ((Kind_List, R.Args)))
              & ", got" & Args'Length'Img;
         end if;
         for I in 1 .. R.Args.Length - 2 loop
            Env.Set (R.Args.Element (I).Symbol, Args (Args'First + I - 1));
         end loop;
         Env.Set (R.Args.Element (R.Args.Length).Symbol,
            Lists.List (Args (Args'First + R.Args.Length - 2 .. Args'Last)));
      else
         if Args'Length /= R.Args.Length then
            raise Argument_Error with "expected "
              & ASU.To_String (Printer.Pr_Str ((Kind_List, R.Args)))
              & ", got" & Args'Length'Img;
         end if;
         for I in 1 .. R.Args.Length loop
            Env.Set (R.Args.Element (I).Symbol, Args (Args'First + I - 1));
         end loop;
      end if;
   end Set_Binds;

   procedure Set_Binds (Item : in Ptr;
                        Env  : in Environments.Ptr;
                        Args : in Lists.Ptr) is
      R : Rec renames Item.Ref.all;
   begin
      if R.Varargs then
         if Args.Length - 1 < R.Args.Length - 2 then
            raise Argument_Error with "expected "
              & ASU.To_String (Printer.Pr_Str ((Kind_List, R.Args)))
              & ", got" & Natural'Image (Args.Length - 1);
         end if;
         for I in 1 .. R.Args.Length - 2 loop
            Env.Set (R.Args.Element (I).Symbol, Args.Element (1 + I));
         end loop;
         Env.Set (R.Args.Element (R.Args.Length).Symbol,
                  Lists.Slice (Args, R.Args.Length));
      else
         if Args.Length - 1 /= R.Args.Length then
            raise Argument_Error with "expected "
              & ASU.To_String (Printer.Pr_Str ((Kind_List, R.Args)))
              & ", got" & Natural'Image (Args.Length - 1);
         end if;
         for I in 1 .. R.Args.Length loop
            Env.Set (R.Args.Element (I).Symbol, Args.Element (1 + I));
         end loop;
      end if;
   end Set_Binds;

   function With_Meta (Data : in Ptr;
                       Meta : in Mal.T)
                      return Mal.T is
      Old : Rec renames Data.Ref.all;
      Ref : Acc;
   begin
      pragma Assert (0 < Old.Refs);
      if Old.Refs = 1 then
         Ref := Data.Ref;
         Old.Refs := 2;
         Old.Meta := Meta;
      else
         Ref := new Rec'(Args    => Data.Ref.all.Args,
                         Expr    => Data.Ref.all.Expr,
                         Env     => Data.Ref.all.Env,
                         Varargs => Data.Ref.all.Varargs,
                         Meta    => Meta,
                         others  => <>);

      end if;
      return (Kind_Function, (AFC with Ref));
   end With_Meta;

end Types.Functions;
