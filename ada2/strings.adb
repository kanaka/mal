with Ada.Strings.Hash;

package body Strings is

   Dict       : Sets.Set;
   Empty_Hash : constant Ada.Containers.Hash_Type := Ada.Strings.Hash ("");

   ----------------------------------------------------------------------

   procedure Adjust (Object : in out Ptr) is
   begin
      if Sets.Has_Element (Object.Position) then
         Dict (Object.Position).Refs := Dict (Object.Position).Refs + 1;
      end if;
   end Adjust;

   function Alloc (Source : in String) return Ptr
   is
      Inserted : Boolean;
      Position : Sets.Cursor;
   begin
      if Source /= "" then
         Sets.Insert (Dict,
                      (Data => Source,
                       Hash => Ada.Strings.Hash (Source),
                       Last => Source'Length,
                       Refs => 1),
                      Position,
                      Inserted);
         if not Inserted then
            Dict (Position).Refs := Dict (Position).Refs + 1;
         end if;
      end if;
      return (Ada.Finalization.Controlled with Position => Position);
   end Alloc;

   function Deref (Source : in Ptr) return String is
      (if Sets.Has_Element (Source.Position)
         then Dict (Source.Position).Data
         else "");

   procedure Finalize (Object : in out Ptr)
   is
      Refs : Positive;
   begin
      if Sets.Has_Element (Object.Position) then
         Refs := Dict (Object.Position).Refs;
         if 1 < Refs then
            Dict (Object.Position).Refs := Refs - 1;
            Object.Position := Sets.No_Element;
         else
            Sets.Delete (Dict, Object.Position);
         end if;
      end if;
   end Finalize;

   function Hash (Source : in Ptr) return Ada.Containers.Hash_Type is
      (if Sets.Has_Element (Source.Position)
         then Dict (Source.Position).Hash
         else Empty_Hash);

end Strings;
