with Err;
with Types.Builtins;
with Types.Fns;

package body Types.Atoms is

   function Atom (Args : in T_Array) return T is
   begin
      Err.Check (Args'Length = 1, "expected 1 parameter");
      declare
         Ref : constant Atom_Ptr := new Instance;
      begin
         Garbage_Collected.Register (Garbage_Collected.Pointer (Ref));
         Ref.all.Data := Args (Args'First);
         return (Kind_Atom, Ref);
      end;
   end Atom;

   function Deref (Args : in T_Array) return T is
   begin
      Err.Check (Args'Length = 1 and then Args (Args'First).Kind = Kind_Atom,
                 "expected an atom");
      return Args (Args'First).Atom.all.Data;
   end Deref;

   function Deref (Item : in Instance) return T
   is (Item.Data);

   procedure Keep_References (Object : in out Instance) is
   begin
      Keep (Object.Data);
      Keep (Object.Meta);
   end Keep_References;

   function Meta (Item : in Instance) return T
   is (Item.F_Meta);

   function Reset (Args : in T_Array) return T is
   begin
      Err.Check (Args'Length = 2 and then Args (Args'First).Kind = Kind_Atom,
                 "expected an atom then a value");
      Args (Args'First).Atom.all.Data := Args (Args'Last);
      return Args (Args'Last);
   end Reset;

   function Swap (Args : in T_Array) return T is
   begin
      Err.Check (2 <= Args'Length and then Args (Args'First).Kind = Kind_Atom,
                 "expected an atom, a function, then optional arguments");
      declare
         X : T renames Args (Args'First).Atom.all.Data;
         F : T renames Args (Args'First + 1);
         A : constant T_Array := X & Args (Args'First + 2 .. Args'Last);
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

   function With_Meta (Item     : in Instance;
                       Metadata : in T) return T is
      Ref : constant Atom_Ptr := new Instance;
   begin
      Garbage_Collected.Register (Garbage_Collected.Pointer (Ref));
      Ref.all.Data := Item.Data;
      Ref.all.F_Meta := Metadata;
      return (Kind_Atom, Ref);
   end With_Meta;

end Types.Atoms;
