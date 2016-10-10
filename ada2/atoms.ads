private with Ada.Finalization;
limited with Types;

package Atoms is

   --  Equivalent to a Lists.Ptr with zero or one elements.

   type Ptr is tagged private;
   No_Element : constant Ptr;

   function Alloc (New_Value : in Types.Mal_Type) return Ptr
     with Inline;

   function Deref (Container : in Ptr) return Types.Mal_Type
     with Inline, Pre => Container /= No_Element;

   procedure Set (Container : in Ptr;
                  New_Value : in Types.Mal_Type)
     with Inline, Pre => Container /= No_Element;

private

   type Atom_Record;
   type Atom_Access is access Atom_Record;
   type Ptr is new Ada.Finalization.Controlled with record
      Ref : Atom_Access := null;
   end record;
   overriding procedure Adjust (Object : in out Ptr)
     with Inline;
   overriding procedure Finalize (Object : in out Ptr)
     with Inline;
   --  Predefined equality is fine.

   No_Element : constant Ptr := (Ada.Finalization.Controlled with Ref => null);

end Atoms;
