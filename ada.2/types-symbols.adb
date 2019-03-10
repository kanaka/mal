with Ada.Containers.Ordered_Sets;
with Ada.Strings.Hash;
with Ada.Unchecked_Deallocation;

package body Types.Symbols is

   --  For the global dictionnary of symbols, an ordered set seems
   --  better than a hash map.

   type Rec (Last : Positive) is limited record
      Refs : Natural;
      Hash : Ada.Containers.Hash_Type;
      Data : String (1 .. Last);
   end record;
   procedure Free is new Ada.Unchecked_Deallocation (Rec, Acc);

   function "<" (Left, Right : in Acc) return Boolean with Inline;
   function Eq  (Left, Right : in Acc) return Boolean with Inline;
   --  It would be unwise to name this function "=" and override the
   --  predefined equality for Acc.
   --  We only search by key and insert new elements, so this should
   --  always return False.
   package Sets is new Ada.Containers.Ordered_Sets (Element_Type => Acc,
                                                    "<"          => "<",
                                                    "="          => Eq);

   function Key (Item : in Acc) return String with Inline;
   package Keys is new Sets.Generic_Keys (Key_Type => String,
                                          Key      => Key,
                                          "<"      => Standard."<");

   Dict : Sets.Set;

   ----------------------------------------------------------------------

   function "<" (Left, Right : in Acc) return Boolean
   is (Left.all.Data < Right.all.Data);

   procedure Adjust (Object : in out Ptr) is
   begin
      Object.Ref.all.Refs := Object.Ref.all.Refs + 1;
   end Adjust;

   function Constructor (Source : in String) return Ptr is
      Position : constant Sets.Cursor := Keys.Find (Dict, Source);
      Ref      : Acc;
   begin
      if Sets.Has_Element (Position) then
         Ref := Sets.Element (Position);
         Ref.all.Refs := Ref.all.Refs + 1;
      else
         Ref := new Rec'(Data => Source,
                         Hash => Ada.Strings.Hash (Source),
                         Last => Source'Length,
                         Refs => 1);
         Dict.Insert (Ref);
      end if;
      return (Ada.Finalization.Controlled with Ref);
   end Constructor;

   function Eq (Left, Right : in Acc) return Boolean is
   begin
      pragma Assert (Left /= Right);
      pragma Assert (Left.all.Data /= Right.all.Data);
      return False;
   end Eq;

   procedure Finalize (Object : in out Ptr) is
   begin
      if Object.Ref /= null and then 0 < Object.Ref.all.Refs then
         Object.Ref.all.Refs := Object.Ref.all.Refs - 1;
         if 0 < Object.Ref.all.Refs then
            Object.Ref := null;
         else
            Dict.Delete (Object.Ref);
            Free (Object.Ref);
         end if;
      end if;
   end Finalize;

   function Hash (Item : in Ptr) return Ada.Containers.Hash_Type
   is (Item.Ref.all.Hash);

   function Key (Item : in Acc) return String
   is (Item.all.Data);

   function To_String (Item : in Ptr) return String
   is (Item.Ref.all.Data);

   function To_String (Item : in Symbol_Array) return String is
      I : Natural := Item'Length + 1;
   begin
      for S of Item loop
         I := I + S.Ref.all.Last;
      end loop;
      return R : String (1 .. I) do
         R (1) := '(';
         I := 2;
         for S of Item loop
            if 2 < I then
               R (I) := ' ';
               I := I + 1;
            end if;
            R (I .. I + S.Ref.all.Last - 1) := S.Ref.all.Data;
            I := I + S.Ref.all.Last;
         end loop;
         pragma Assert (I = R'Last);
         R (R'Last) := ')';
      end return;
   end To_String;

end Types.Symbols;
