with Ada.Unchecked_Deallocation;
with Types;

package body Atoms is

   type Atom_Record is limited record
      Data : Types.Mal_Type;
      Refs : Positive;
   end record;

   procedure Free is new Ada.Unchecked_Deallocation (Object => Atom_Record,
                                                     Name   => Atom_Access);

   ----------------------------------------------------------------------

   procedure Adjust (Object : in out Ptr) is
   begin
      if Object.Ref /= null then
         Object.Ref.all.Refs := Object.Ref.all.Refs + 1;
      end if;
   end Adjust;

   function Alloc (New_Value : in Types.Mal_Type) return Ptr
   is (Ada.Finalization.Controlled with
         Ref => new Atom_Record'(Data => New_Value,
                                 Refs => 1));

   function Deref (Container : in Ptr) return Types.Mal_Type is
      (Container.Ref.all.Data);

   procedure Finalize (Object : in out Ptr)
   is
      Refs : Positive;
   begin
      if Object.Ref /= null then
         Refs := Object.Ref.all.Refs;
         if 1 < Refs then
            Object.Ref.all.Refs := Refs - 1;
            Object.Ref := null;
         else
            Free (Object.Ref);
         end if;
      end if;
   end Finalize;

   procedure Set (Container : in Ptr;
                  New_Value : in Types.Mal_Type) is
   begin
      Container.Ref.all.Data := New_Value;
   end Set;

end Atoms;
