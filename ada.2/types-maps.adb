with Ada.Strings.Unbounded.Hash;

with Err;
with Types.Sequences;

package body Types.Maps is

   function Constructor return Mal.Map_Ptr with Inline;

   ----------------------------------------------------------------------

   function "=" (Left, Right : in Instance) return Boolean
   is (Left.Data = Right.Data);

   function Assoc (Args : in Mal.T_Array) return Mal.T is
      Ref : constant Mal.Map_Ptr := Constructor;
   begin
      Err.Check (Args'Length mod 2 = 1, "expected an odd parameter count");
      Err.Check (Args (Args'First).Kind = Kind_Map,
                 "parameter 1 must be a map");
      Ref.all.Data := Args (Args'First).Map.all.Data;
      for I in 1 .. Args'Length / 2 loop
         Ref.all.Data.Include (Key      => Args (Args'First + 2 * I - 1),
                               New_Item => Args (Args'First + 2 * I));
         --  This call checks the kind of the key.
      end loop;
      return (Kind_Map, Ref);
   end Assoc;

   function Contains (Args : in Mal.T_Array) return Mal.T is
   begin
      Err.Check (Args'Length = 2, "expected 2 parameters");
      Err.Check (Args (Args'First).Kind = Kind_Map,
                 "parameter 1 must be a map");
      return (Kind_Boolean,
              Args (Args'First).Map.all.Data.Contains (Args (Args'Last)));
   end Contains;

   function Constructor return Mal.Map_Ptr is
      Ref : constant Mal.Map_Ptr := new Instance;
   begin
      Garbage_Collected.Register (Garbage_Collected.Pointer (Ref));
      return Ref;
   end Constructor;

   function Dissoc (Args : in Mal.T_Array) return Mal.T is
      Ref : constant Mal.Map_Ptr := Constructor;
   begin
      Err.Check (0 < Args'Length, "expected at least 1 parameter");
      Err.Check (Args (Args'First).Kind = Kind_Map,
                 "parameter 1 must be a map");
      Ref.all.Data := Args (Args'First).Map.all.Data;
      for I in Args'First + 1 .. Args'Last loop
         Ref.all.Data.Exclude (Args (I));
         --  This call checks the kind of the key.
      end loop;
      return (Kind_Map, Ref);
   end Dissoc;

   function Generic_Eval (Container : in Instance;
                          Env       : in Env_Type) return Mal.T
   is
      --  Copy the whole hash in order to avoid recomputing the hash
      --  for each key, even if it implies unneeded calls to adjust
      --  and finalize for Mal_Type values.
      Ref : constant Mal.Map_Ptr := Constructor;
   begin
      Ref.Data := Container.Data;
      for Position in Ref.all.Data.Iterate loop
         Ref.all.Data.Replace_Element (Position,
                                       Eval (HM.Element (Position), Env));
         --  This call may raise exceptions.
      end loop;
      return Mal.T'(Kind_Map, Ref);
   end Generic_Eval;

   function Get (Args : in Mal.T_Array) return Mal.T is
      Position : HM.Cursor;
   begin
      Err.Check (Args'Length = 2, "expected 2 parameters");
      case Args (Args'First).Kind is
         when Kind_Nil =>
            Err.Check (Args (Args'Last).Kind in Kind_Key,
                       "key must be a keyword or string");
            return Mal.Nil;
         when Kind_Map =>
            Position := Args (Args'First).Map.all.Data.Find (Args (Args'Last));
            --  This call checks the kind of the key.
            if HM.Has_Element (Position) then
               return HM.Element (Position);
            else
               return Mal.Nil;
            end if;
         when others =>
            Err.Raise_With  ("parameter 1 must be nil or a map");
      end case;
   end Get;

   function Hash (Item : in Mal.T) return Ada.Containers.Hash_Type is
   begin
      Err.Check (Item.Kind in Kind_Key, "keys must be keywords or strings");
      return (Ada.Strings.Unbounded.Hash (Item.S));
   end Hash;

   function Hash_Map (Args : in Mal.T_Array) return Mal.T is
      Binds : constant Natural := Args'Length / 2;
      Ref   : Mal.Map_Ptr;
   begin
      Err.Check (Args'Length mod 2 = 0, "expected an even parameter count");
      Ref := Constructor;
      Ref.all.Data.Reserve_Capacity (Ada.Containers.Count_Type (Binds));
      for I in 0 .. Binds - 1 loop
         Ref.all.Data.Include (Key      => Args (Args'First + 2 * I),
                               New_Item => Args (Args'First + 2 * I + 1));
         --  This call checks the kind of the key.
      end loop;
      return (Kind_Map, Ref);
   end Hash_Map;

   procedure Iterate (Container : in Instance) is
   begin
      for Position in Container.Data.Iterate loop
         Process (HM.Key (Position), HM.Element (Position));
      end loop;
   end Iterate;

   procedure Keep_References (Object : in out Instance) is
   begin
      for Position in Object.Data.Iterate loop
         Mal.Keep (HM.Key (Position));
         Mal.Keep (HM.Element (Position));
      end loop;
      Mal.Keep (Object.F_Meta);
   end Keep_References;

   function Keys (Args : in Mal.T_Array) return Mal.T is
      A1 : Mal.Map_Ptr;
      R  : Mal.Sequence_Ptr;
      I  : Positive := 1;
   begin
      Err.Check (Args'Length = 1, "expected 1 parameter");
      Err.Check (Args (Args'First).Kind = Kind_Map,
                 "parameter 1 must be a map");
      A1 := Args (Args'First).Map;
      R := Sequences.Constructor (A1.all.Length);
      for Position in A1.all.Data.Iterate loop
         R.all.Replace_Element (I, HM.Key (Position));
         I := I + 1;
      end loop;
      return (Kind_List, R);
   end Keys;

   function Length (Container : in Instance) return Natural
   is (Natural (Container.Data.Length));

   function Meta (Container : in Instance) return Mal.T
   is (Container.F_Meta);

   function Vals (Args : in Mal.T_Array) return Mal.T is
      A1 : Mal.Map_Ptr;
      R  : Mal.Sequence_Ptr;
      I  : Positive := 1;
   begin
      Err.Check (Args'Length = 1, "expected 1 parameter");
      Err.Check (Args (Args'First).Kind = Kind_Map,
                 "parameter 1 must be a map");
      A1 := Args (Args'First).Map;
      R := Sequences.Constructor (A1.all.Length);
      for Element of A1.all.Data loop
         R.all.Replace_Element (I, Element);
         I := I + 1;
      end loop;
      return (Kind_List, R);
   end Vals;

   function With_Meta (Data     : in Instance;
                       Metadata : in Mal.T) return Mal.T
   is
      Ref : constant Mal.Map_Ptr := Constructor;
   begin
      Ref.all.Data := Data.Data;
      Ref.all.F_Meta := Metadata;
      return (Kind_Map, Ref);
   end With_Meta;

end Types.Maps;
