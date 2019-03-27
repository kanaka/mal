with Ada.Unchecked_Deallocation;

with Err;
with Types.Mal;

package body Types.Atoms is

   type Rec is limited record
      Refs : Natural;
      Data : Mal.T;
   end record;

   procedure Free is new Ada.Unchecked_Deallocation (Rec, Acc);
   Allocations : Natural := 0;

   ----------------------------------------------------------------------

   procedure Adjust (Object : in out Ptr) is
   begin
      Object.Ref.all.Refs := @ + 1;
   end Adjust;

   function Atom (Args : in Mal.T_Array) return Mal.T is
   begin
      Err.Check (Args'Length = 1, "expected 1 parameter");
      Allocations := Allocations + 1;
      return (Kind_Atom, (Ada.Finalization.Controlled with new Rec'
                            (Refs => 1,
                             Data => Args (Args'First))));
   end Atom;

   procedure Check_Allocations is
   begin
      pragma Assert (Allocations = 0);
   end Check_Allocations;

   function Deref (Args : in Mal.T_Array) return Mal.T is
   begin
      Err.Check (Args'Length = 1, "expected 1 parameter");
      Err.Check (Args (Args'First).Kind = Kind_Atom, "expected an atom");
      return Args (Args'First).Atom.Ref.all.Data;
   end Deref;

   function Deref (Item : in Ptr) return Mal.T
   is (Item.Ref.all.Data);

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

   function Reset (Args : in Mal.T_Array) return Mal.T is
   begin
      Err.Check (Args'Length = 2, "expected 2 parameters");
      Err.Check (Args (Args'First).Kind = Kind_Atom,
                  "parameter 1 must be an atom");
      Args (Args'First).Atom.Ref.all.Data := Args (Args'Last);
      return Args (Args'Last);
   end Reset;

   function Swap (Args : in Mal.T_Array) return Mal.T is
   begin
      Err.Check (2 <= Args'Length, "expected at least 2 parameters");
      Err.Check (Args (Args'First).Kind = Kind_Atom,
                 "parameter 1 must be an atom");
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
            when Kind_Fn =>
               X := F.Fn.Apply (A);
            when others =>
               Err.Raise_With ("parameter 2 must be a function");
         end case;
         return X;
      end;
   end Swap;

end Types.Atoms;
