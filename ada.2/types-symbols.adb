with Ada.Containers.Hashed_Sets;
with Ada.Strings.Hash;
with Ada.Unchecked_Deallocation;

package body Types.Symbols is

   type Rec (Last : Positive) is limited record
      Refs : Natural;
      Data : String (1 .. Last);
   end record;
   procedure Free is new Ada.Unchecked_Deallocation (Rec, Acc);
   Allocations : Natural := 0;

   function Hash (Item : in Acc) return Ada.Containers.Hash_Type with Inline;
   package Sets is new Ada.Containers.Hashed_Sets (Element_Type        => Acc,
                                                   Hash                => Hash,
                                                   Equivalent_Elements => "=",
                                                   "="                 => "=");
   function Key (Item : in Acc) return String with Inline;
   package Keys is new Sets.Generic_Keys (Key_Type        => String,
                                          Key             => Key,
                                          Hash            => Ada.Strings.Hash,
                                          Equivalent_Keys => "=");

   Dict : Sets.Set;

   ----------------------------------------------------------------------

   procedure Adjust (Object : in out Ptr) is
   begin
      Object.Ref.all.Refs := @ + 1;
   end Adjust;

   procedure Check_Allocations is
   begin
      --  See Types.Symbols.Names.
      pragma Assert (Allocations = 15);
   end Check_Allocations;

   function Constructor (Source : in String) return Ptr is
      Position : constant Sets.Cursor := Keys.Find (Dict, Source);
      Ref      : Acc;
   begin
      --  Avoid exceptions until Ref is controlled.
      if Sets.Has_Element (Position) then
         Ref := Sets.Element (Position);
         Ref.all.Refs := Ref.all.Refs + 1;
      else
         Allocations := Allocations + 1;
         Ref := new Rec'(Data => Source,
                         Last => Source'Length,
                         Refs => 1);
         Dict.Insert (Ref);
      end if;
      return (Ada.Finalization.Controlled with Ref);
   end Constructor;

   procedure Finalize (Object : in out Ptr) is
   begin
      if Object.Ref /= null and then 0 < Object.Ref.all.Refs then
         Object.Ref.all.Refs := @ - 1;
         if 0 < Object.Ref.all.Refs then
            Object.Ref := null;
         else
            Dict.Delete (Object.Ref);
            Allocations := Allocations - 1;
            Free (Object.Ref);
         end if;
      end if;
   end Finalize;

   function Hash (Item : in Acc) return Ada.Containers.Hash_Type
   is (Ada.Strings.Hash (Item.all.Data));

   function Hash (Item : in Ptr) return Ada.Containers.Hash_Type
   is (Ada.Strings.Hash (Item.Ref.all.Data));

   function Key (Item : in Acc) return String
   is (Item.all.Data);

   function To_String (Item : in Ptr) return String
   is (Item.Ref.all.Data);

end Types.Symbols;
