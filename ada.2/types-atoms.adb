with Err;

with Types.Builtins;
with Types.Fns;

package body Types.Atoms is

   function Atom (Args : in Mal.T_Array) return Mal.T is
      Ref : Mal.Atom_Ptr;
   begin
      Err.Check (Args'Length = 1, "expected 1 parameter");
      Ref := new Instance'(Garbage_Collected.Instance with
                           Data => Args (Args'First));
      Garbage_Collected.Register (Garbage_Collected.Pointer (Ref));
      return (Kind_Atom, Ref);
   end Atom;

   function Deref (Args : in Mal.T_Array) return Mal.T is
   begin
      Err.Check (Args'Length = 1, "expected 1 parameter");
      Err.Check (Args (Args'First).Kind = Kind_Atom, "expected an atom");
      return Args (Args'First).Atom.all.Data;
   end Deref;

   function Deref (Item : in Instance) return Mal.T
   is (Item.Data);

   procedure Keep_References (Object : in out Instance) is
   begin
      Mal.Keep (Object.Data);
   end Keep_References;

   function Reset (Args : in Mal.T_Array) return Mal.T is
   begin
      Err.Check (Args'Length = 2, "expected 2 parameters");
      Err.Check (Args (Args'First).Kind = Kind_Atom,
                 "parameter 1 must be an atom");
      Args (Args'First).Atom.all.Data := Args (Args'Last);
      return Args (Args'Last);
   end Reset;

   function Swap (Args : in Mal.T_Array) return Mal.T is
   begin
      Err.Check (2 <= Args'Length, "expected at least 2 parameters");
      Err.Check (Args (Args'First).Kind = Kind_Atom,
                 "parameter 1 must be an atom");
      declare
         use type Mal.T_Array;
         X : Mal.T renames Args (Args'First).Atom.all.Data;
         F : Mal.T renames Args (Args'First + 1);
         A : constant Mal.T_Array := X & Args (Args'First + 2 .. Args'Last);
      begin
         case F.Kind is
            when Kind_Builtin =>
               X := F.Builtin.all (A);
            when Kind_Builtin_With_Meta =>
               X := F.Builtin_With_Meta.all.Builtin.all (A);
            when Kind_Fn =>
               X := F.Fn.all.Apply (A);
            when others =>
               Err.Raise_With ("parameter 2 must be a function");
         end case;
         return X;
      end;
   end Swap;

end Types.Atoms;
