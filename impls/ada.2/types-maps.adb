with Err;
with Types.Sequences;
with Types.Strings;

package body Types.Maps is

   use type HM.Map;

   function Assoc (Initial : in HM.Map;
                   Bind    : in T_Array) return T;

   function Constructor return Map_Ptr with Inline;

   ----------------------------------------------------------------------

   function "=" (Left, Right : in Instance) return Boolean
   is (Left.Data = Right.Data);

   function Assoc (Initial : in HM.Map;
                   Bind    : in T_Array) return T
   is
   begin
      Err.Check (Bind'Length mod 2 = 0, "expected an even bind count");
      declare
         Len : constant Natural := Bind'Length / 2;
         Ref : constant Map_Ptr := Constructor;
      begin
         Ref.all.Data := Initial;
         for I in 0 .. Len - 1 loop
            Ref.all.Data.Include (Bind (Bind'First + 2 * I),
                                  Bind (Bind'First + 2 * I + 1));
         end loop;
         return (Kind_Map, Ref);
      end;
   end Assoc;

   function Assoc (Args : in T_Array) return T is
   begin
      Err.Check (0 < Args'Length and then Args (Args'First).Kind = Kind_Map,
                 "first parameter must be a map");
      return Assoc (Args (Args'First).Map.all.Data,
                    Args (Args'First + 1 .. Args'Last));
   end Assoc;

   function Constructor return Map_Ptr is
      Ref : constant Map_Ptr := new Instance;
   begin
      Garbage_Collected.Register (Garbage_Collected.Pointer (Ref));
      return Ref;
   end Constructor;

   function Contains (Args : in T_Array) return T is
   begin
      Err.Check (Args'Length = 2 and then Args (Args'First).Kind = Kind_Map,
                 "expected a map then a key");
      return (Kind_Boolean,
              Args (Args'First).Map.all.Data.Contains (Args (Args'Last)));
   end Contains;

   function Dissoc (Args : in T_Array) return T is
   begin
      Err.Check (0 < Args'Length and then Args (Args'First).Kind = Kind_Map,
                 "expected a map then keys");
      declare
         Ref : constant Map_Ptr := Constructor;
      begin
         Ref.all.Data := Args (Args'First).Map.all.Data;
         for I in Args'First + 1 .. Args'Last loop
            Ref.all.Data.Exclude (Args (I));
            --  This call checks the kind of the key.
         end loop;
         return (Kind_Map, Ref);
      end;
   end Dissoc;

   function Element (Position  : in Cursor) return T
   is (HM.Element (HM.Cursor (Position)));

   function First (Container : in Instance) return Cursor
   is (Cursor (Container.Data.First));

   function Get (Args : in T_Array) return T is
   begin
      Err.Check (Args'Length = 2, "expected 2 parameters");
      case Args (Args'First).Kind is
         when Kind_Nil =>
            Err.Check (Args (Args'Last).Kind in Kind_Key,
                       "key must be a keyword or string");
            return Nil;
         when Kind_Map =>
            declare
               Position : constant HM.Cursor
                 := Args (Args'First).Map.all.Data.Find (Args (Args'Last));
            begin
               if HM.Has_Element (Position) then
                  return HM.Element (Position);
               else
                  return Nil;
               end if;
            end;
         when others =>
            Err.Raise_With  ("parameter 1 must be nil or a map");
      end case;
   end Get;

   function Has_Element (Position : in Cursor) return Boolean
   is (HM.Has_Element (HM.Cursor (Position)));

   function Hash (Item : in T) return Ada.Containers.Hash_Type is
   begin
      Err.Check (Item.Kind in Kind_Key, "keys must be keywords or strings");
      return Strings.Hash (Item.Str);
   end Hash;

   function Hash_Map (Args : in T_Array) return T
   is (Assoc (HM.Empty_Map, Args));

   procedure Keep_References (Object : in out Instance) is
   begin
      for Position in Object.Data.Iterate loop
         Keep (HM.Key (Position));
         Keep (HM.Element (Position));
      end loop;
      Keep (Object.F_Meta);
   end Keep_References;

   function Key (Position  : in Cursor) return T
   is (HM.Key (HM.Cursor (Position)));

   function Keys (Args : in T_Array) return T is
   begin
      Err.Check (Args'Length = 1 and then Args (Args'First).Kind = Kind_Map,
                 "expected a map");
      declare
         A1       : HM.Map renames Args (Args'First).Map.all.Data;
         Ref      : constant Sequence_Ptr
           := Sequences.Constructor (Natural (A1.Length));
         I        : Positive := 1;
      begin
         for Position in A1.Iterate loop
            Ref.all.Data (I) := HM.Key (Position);
            I := I + 1;
         end loop;
         return (Kind_List, Ref);
      end;
   end Keys;

   function Meta (Container : in Instance) return T
   is (Container.F_Meta);

   procedure Next (Position : in out Cursor) is
   begin
      HM.Next (HM.Cursor (Position));
   end Next;

   function New_Map (Source : in Instance) return T
   is
      Ref : constant Map_Ptr := Constructor;
   begin
      Ref.all.Data := Source.Data;
      return (Kind_Map, Ref);
   end New_Map;

   procedure Replace_Element (Container : in out Instance;
                              Position  : in     Cursor;
                              New_Item  : in     T)
   is
   begin
      Container.Data.Replace_Element (HM.Cursor (Position), New_Item);
   end Replace_Element;

   function Vals (Args : in T_Array) return T is
   begin
      Err.Check (Args'Length = 1 and then Args (Args'First).Kind = Kind_Map,
                 "expected a map");
      declare
         A1 : HM.Map renames Args (Args'First).Map.all.Data;
         R  : constant Sequence_Ptr
           := Sequences.Constructor (Natural (A1.Length));
         I  : Positive              := 1;
      begin
         for Element of A1 loop
            R.all.Data (I) := Element;
            I := I + 1;
         end loop;
         return (Kind_List, R);
      end;
   end Vals;

   function With_Meta (Container : in Instance;
                       Metadata  : in T) return T
   is
      Ref : constant Map_Ptr := Constructor;
   begin
      Ref.all.Data := Container.Data;
      Ref.all.F_Meta := Metadata;
      return (Kind_Map, Ref);
   end With_Meta;

end Types.Maps;
