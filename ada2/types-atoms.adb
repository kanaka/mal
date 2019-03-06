with Ada.Unchecked_Deallocation;

with Printer;
with Types.Mal;

package body Types.Atoms is

   type Rec is limited record
      Refs : Natural;
      Data : Mal.T;
   end record;

   procedure Free is new Ada.Unchecked_Deallocation (Rec, Acc);

   ----------------------------------------------------------------------

   procedure Adjust (Object : in out Ptr) is
   begin
      Object.Ref.all.Refs := Object.Ref.all.Refs + 1;
   end Adjust;

   function Atom (Args : in Mal.T_Array) return Mal.T
   is (if Args'Length /= 1 then
          raise Argument_Error with "atom: expects 1 argument"
        else
           (Kind_Atom, (Ada.Finalization.Controlled with new Rec'
                          (Refs => 1,
                           Data => Args (Args'First)))));

   function Deref (Args : in Mal.T_Array) return Mal.T
   is (if Args'Length /= 1 then
          raise Argument_Error with "deref: expects 1 argument"
       elsif Args (Args'First).Kind /= Kind_Atom then
          raise Argument_Error with "deref: expects an atom"
       else
          Args (Args'First).Atom.Ref.all.Data);

   function Deref (Item : in Ptr) return Mal.T
   is (Item.Ref.all.Data);

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

   function Reset (Args : in Mal.T_Array) return Mal.T is
   begin
      if Args'Length /= 2 then
         raise Argument_Error with "reset: expects 2 arguments";
      elsif Args (Args'First).Kind /= Kind_Atom then
         raise Argument_Error with "reset: first argument must be an atom";
      end if;
      Args (Args'First).Atom.Ref.all.Data := Args (Args'Last);
      return Args (Args'Last);
   end Reset;

   function Swap (Args : in Mal.T_Array) return Mal.T is
   begin
      if Args'Length < 2 then
         raise Argument_Error with "swap!: expects at least 2 arguments";
      elsif Args (Args'First).Kind /= Kind_Atom then
         raise Argument_Error with "swap!: first argument must be an atom";
      end if;
      declare
         use type Mal.T_Array;
         X : Mal.T renames Args (Args'First).Atom.Ref.all.Data;
         F : Mal.T renames Args (Args'First + 1);
         A : constant Mal.T_Array := X & Args (Args'First + 2 .. Args'Last);
      begin
         case F.Kind is
            when Kind_Builtin =>
               X := F.Builtin.all (A);
            when Kind_Builtin_With_Meta =>
               X := F.Builtin_With_Meta.Builtin.all (A);
            when Kind_Function =>
               X := F.Fn.Apply (A);
            when others =>
               raise Argument_Error
                 with "swap!: cannot call " & Printer.Img (F);
         end case;
         return X;
      end;
   end Swap;

end Types.Atoms;
