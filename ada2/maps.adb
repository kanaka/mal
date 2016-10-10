with Ada.Containers.Hashed_Maps;
with Ada.Unchecked_Deallocation;
with Strings;
with Types;

package body Maps is

   function Hash (Item : in Types.Mal_Type) return Ada.Containers.Hash_Type
     with Inline, Pre => Item.Kind in Types.Kind_String | Types.Kind_Keyword;

   package Hashed_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Types.Mal_Type,
      Element_Type    => Types.Mal_Type,
      Hash            => Hash,
      Equivalent_Keys => Types."=",
      "="             => Types."=");

   type Map_Record is limited record
      Data : Hashed_Maps.Map;
      Refs : Positive;
   end record;

   procedure Free is new Ada.Unchecked_Deallocation (Object => Map_Record,
                                                     Name   => Map_Access);

   use type Ada.Containers.Count_Type;

   ----------------------------------------------------------------------

   function "=" (Left, Right : in Ptr) return Boolean is
      (Hashed_Maps."=" (Left.Ref.all.Data, Right.Ref.all.Data));

   procedure Adjust (Object : in out Ptr) is
   begin
      Object.Ref.all.Refs := Object.Ref.all.Refs + 1;
   end Adjust;

   function Assoc (Container : in Ptr;
                   Pairs     : in Types.Mal_Type_Array) return Ptr
   is
      pragma Assert (Pairs'Length mod 2 = 0);
      Pair_Count : constant Ada.Containers.Count_Type
        := Ada.Containers.Count_Type (Pairs'Length) / 2;
      Result     : Ptr;
   begin
      Result.Ref.all.Data.Reserve_Capacity (Pair_Count
                                              + Container.Ref.all.Data.Length);
      Result.Ref.all.Data.Assign (Container.Ref.all.Data);
      for I in 0 .. Pairs'Length / 2 - 1 loop
         pragma Assert (Pairs (Pairs'First + 2 * I).Kind in Types.Kind_String
                          | Types.Kind_Keyword);
         Result.Ref.all.Data.Include (Pairs (Pairs'First + 2 * I),
                                      Pairs (Pairs'First + 2 * I + 1));
      end loop;
      return Result;
   end Assoc;

   function Contains (Container : in Ptr;
                      Key       : in Types.Mal_Type) return Boolean is
      (Container.Ref.all.Data.Contains (Key));

   function Dissoc (Source : in Ptr;
                    Keys   : in Types.Mal_Type_Array) return Ptr
   is
      Result : Ptr;
   begin
      Result.Ref.all.Data.Assign (Source.Ref.all.Data);
      for I in Keys'Range loop
         pragma Assert (Keys (I).Kind in Types.Kind_String
                          | Types.Kind_Keyword);
         Result.Ref.all.Data.Exclude (Keys (I));
      end loop;
      return Result;
   end Dissoc;

   procedure Finalize (Object : in out Ptr)
   is
      Refs : Positive;
   begin
      --  Finalize may be called twice.
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

   procedure Iterate
     (Container : in Ptr;
      Process   : not null access procedure (Key     : in Types.Mal_Type;
                                             Element : in Types.Mal_Type)) is
   begin
      for Position in Container.Ref.all.Data.Iterate loop
         Process.all (Hashed_Maps.Key (Position),
                      Hashed_Maps.Element (Position));
      end loop;
   end Iterate;

   function Get (Container : in Ptr;
                 Key       : in Types.Mal_Type) return Types.Mal_Type
   is
      Position : Hashed_Maps.Cursor;
   begin
      Position := Container.Ref.all.Data.Find (Key);
      if Hashed_Maps.Has_Element (Position) then
         return Hashed_Maps.Element (Position);
      end if;
      raise Unknown_Key with "'" & Key.S.Deref & "' not found";
   end Get;

   function Hash (Item : in Types.Mal_Type) return Ada.Containers.Hash_Type is
      (Item.S.Hash);

   function Hash_Map (Pairs : in Types.Mal_Type_Array) return Ptr
   is
      pragma Assert (Pairs'Length mod 2 = 0);
      Pair_Count : constant Ada.Containers.Count_Type
        := Ada.Containers.Count_Type (Pairs'Length) / 2;
      Result : Ptr;
   begin
      Result.Ref.all.Data.Reserve_Capacity (Pair_Count);
      for I in 0 .. Pairs'Length / 2 - 1 loop
         pragma Assert (Pairs (Pairs'First + 2 * I).Kind in Types.Kind_String
                          | Types.Kind_Keyword);
         Result.Ref.all.Data.Include (Pairs (Pairs'First + 2 * I),
                                      Pairs (Pairs'First + 2 * I + 1));
      end loop;
      return Result;
   end Hash_Map;

   procedure Initialize (Object : in out Ptr) is
   begin
      Object.Ref := new Map_Record'(Data => Hashed_Maps.Empty_Map,
                                    Refs => 1);
   end Initialize;

   function Length (Container : in Ptr) return Natural
   is (Natural (Container.Ref.all.Data.Length));

   function Map (Container : in Ptr;
                 F         : not null access function (X : in Types.Mal_Type)
                                                      return Types.Mal_Type)
                return Ptr
   is
      Result : Ptr;
   begin
      Result.Ref.all.Data.Assign (Container.Ref.all.Data);
      --  Ensure the invariants before calling F, in case it raises exceptions.
      for Position in Result.Ref.all.Data.Iterate loop
         Result.Ref.all.Data.Replace_Element
           (Position, F.all (Hashed_Maps.Element (Position)));
      end loop;
      return Result;
   end Map;

end Maps;
