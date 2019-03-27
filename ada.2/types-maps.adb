with Ada.Containers.Hashed_Maps;
with Ada.Strings.Unbounded.Hash;
with Ada.Unchecked_Deallocation;

with Err;
with Types.Sequences;
with Types.Mal;

package body Types.Maps is

   subtype AFC is Ada.Finalization.Controlled;
   use type Ada.Containers.Count_Type;

   function Hash (Item : in Mal.T) return Ada.Containers.Hash_Type
     with Inline;
   --  This function also checks the kind of the key, and raise an
   --  error in case of problem.

   package HM is new Ada.Containers.Hashed_Maps (Key_Type        => Mal.T,
                                                 Element_Type    => Mal.T,
                                                 Hash            => Hash,
                                                 Equivalent_Keys => Mal."=",
                                                 "="             => Mal."=");
   use type HM.Map;

   type Rec is limited record
      Refs : Natural := 1;
      Data : HM.Map  := HM.Empty_Map;
      Meta : Mal.T   := Mal.Nil;
   end record;

   procedure Free is new Ada.Unchecked_Deallocation (Rec, Acc);
   Allocations : Natural := 0;

   ----------------------------------------------------------------------

   function "=" (Left, Right : in Ptr) return Boolean
   is (Left.Ref.all.Data = Right.Ref.all.Data);

   procedure Adjust (Object : in out Ptr) is
   begin
      Object.Ref.all.Refs := @ + 1;
   end Adjust;

   function Assoc (Args : in Mal.T_Array) return Mal.T is
      Ref : Acc;
   begin
      Err.Check (Args'Length mod 2 = 1, "expected an odd parameter count");
      Err.Check (Args (Args'First).Kind = Kind_Map,
                 "parameter 1 must be a map");
      --  Avoid exceptions until Ref is controlled.
      Ref := Args (Args'First).Map.Ref;
      pragma Assert (0 < Ref.all.Refs);
      if Ref.all.Refs = 1 then
         Ref.all.Refs := 2;
         Ref.all.Meta := Mal.Nil;
      else
         Allocations := Allocations + 1;
         Ref := new Rec'(Data   => Ref.all.Data,
                         others => <>);
      end if;
      return R : constant Mal.T := (Kind_Map, (AFC with Ref)) do
         for I in 1 .. Args'Length / 2 loop
            Ref.all.Data.Include (Key      => Args (Args'First + 2 * I - 1),
                                  New_Item => Args (Args'First + 2 * I));
            --  This call checks the kind of the key.
         end loop;
      end return;
   end Assoc;

   procedure Check_Allocations is
   begin
      pragma Assert (Allocations = 0);
   end Check_Allocations;

   function Contains (Args : in Mal.T_Array) return Mal.T is
   begin
      Err.Check (Args'Length = 2, "expected 2 parameters");
      Err.Check (Args (Args'First).Kind = Kind_Map,
                 "parameter 1 must be a map");
      return (Kind_Boolean,
              Args (Args'First).Map.Ref.all.Data.Contains (Args (Args'Last)));
   end Contains;

   function Dissoc (Args : in Mal.T_Array) return Mal.T is
      Ref : Acc;
   begin
      Err.Check (0 < Args'Length, "expected at least 1 parameter");
      Err.Check (Args (Args'First).Kind = Kind_Map,
                 "parameter 1 must be a map");
      --  Avoid exceptions until Ref is controlled.
      Ref := Args (Args'First).Map.Ref;
      pragma Assert (0 < Ref.all.Refs);
      if Ref.all.Refs = 1 then
         Ref.all.Refs := 2;
         Ref.all.Meta := Mal.Nil;
      else
         Allocations := Allocations + 1;
         Ref := new Rec'(Data   => Ref.all.Data,
                         others => <>);
      end if;
      return R : constant Mal.T := (Kind_Map, (AFC with Ref)) do
         for I in Args'First + 1 .. Args'Last loop
            Ref.all.Data.Exclude (Args (I));
            --  This call checks the kind of the key.
         end loop;
      end return;
   end Dissoc;

   procedure Finalize (Object : in out Ptr) is
   begin
      if Object.Ref /= null and then 0 < Object.Ref.all.Refs then
         Object.Ref.all.Refs := @ - 1;
         if 0 < Object.Ref.all.Refs then
            Object.Ref := null;
         else
            Allocations := Allocations - 1;
            Free (Object.Ref);
         end if;
      end if;
   end Finalize;

   function Generic_Eval (Container : in Ptr;
                          Env       : in Env_Type)
                         return Mal.T
   is
      --  Copy the whole hash in order to avoid recomputing the hash
      --  for each key, even if it implies unneeded calls to adjust
      --  and finalize for Mal_Type values.
      --  Avoid exceptions until Ref is controlled.
      Ref : Acc := Container.Ref;
   begin
      pragma Assert (0 < Ref.all.Refs);
      if Ref.all.Refs = 1 then
         Ref.all.Refs := 2;
         Ref.all.Meta := Mal.Nil;
      else
         Allocations := Allocations + 1;
         Ref := new Rec'(Data   => Ref.all.Data,
                         others => <>);
      end if;
      return R : constant Mal.T := (Kind_Map, (AFC with Ref)) do
         for Position in Ref.all.Data.Iterate loop
            Ref.all.Data.Replace_Element (Position,
               Eval (HM.Element (Position), Env));
            --  This call may raise exceptions.
         end loop;
      end return;
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
            Position
              := Args (Args'First).Map.Ref.all.Data.Find (Args (Args'Last));
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
      Ref   : Acc;
   begin
      Err.Check (Args'Length mod 2 = 0, "expected an even parameter count");
      Allocations := Allocations + 1;
      --  Avoid exceptions until Ref is controlled.
      Ref := new Rec;
      Ref.all.Data.Reserve_Capacity (Ada.Containers.Count_Type (Binds));
      return R : constant Mal.T := (Kind_Map, (AFC with Ref)) do
         for I in 0 .. Binds - 1 loop
            Ref.all.Data.Include (Key      => Args (Args'First + 2 * I),
                                  New_Item => Args (Args'First + 2 * I + 1));
            --  This call checks the kind of the key.
         end loop;
      end return;
   end Hash_Map;

   procedure Iterate (Container : in Ptr) is
   begin
      for Position in Container.Ref.all.Data.Iterate loop
         Process (HM.Key (Position), HM.Element (Position));
      end loop;
   end Iterate;

   function Keys (Args : in Mal.T_Array) return Mal.T is
   begin
      Err.Check (Args'Length = 1, "expected 1 parameter");
      Err.Check (Args (Args'First).Kind = Kind_Map,
                 "parameter 1 must be a map");
      declare
         A1 : HM.Map renames Args (Args'First).Map.Ref.all.Data;
         R  : Mal.T_Array (1 .. Natural (A1.Length));
         I  : Positive := 1;
      begin
         for Position in A1.Iterate loop
            R (I) := HM.Key (Position);
            I := I + 1;
         end loop;
         return Sequences.List (R);
      end;
   end Keys;

   function Meta (Container : in Ptr) return Mal.T
   is (Container.Ref.all.Meta);

   function Vals (Args : in Mal.T_Array) return Mal.T is
   begin
      Err.Check (Args'Length = 1, "expected 1 parameter");
      Err.Check (Args (Args'First).Kind = Kind_Map,
                 "parameter 1 must be a map");
      declare
         A1 : HM.Map renames Args (Args'First).Map.Ref.all.Data;
         R  : Mal.T_Array (1 .. Natural (A1.Length));
         I  : Positive := 1;
      begin
         for Element of A1 loop
            R (I) := Element;
            I := I + 1;
         end loop;
         return Sequences.List (R);
      end;
   end Vals;

   function With_Meta (Data     : in Ptr;
                       Metadata : in Mal.T)
                      return Mal.T
   is
      --  Avoid exceptions until Ref is controlled.
      Ref : Acc := Data.Ref;
   begin
      pragma Assert (0 < Ref.all.Refs);
      if Ref.all.Refs = 1 then
         Ref.all.Refs := 2;
         Ref.all.Meta := Metadata;
      else
         Allocations := Allocations + 1;
         Ref := new Rec'(Data   => Ref.all.Data,
                         Meta   => Metadata,
                         others => <>);
      end if;
      return (Kind_Map, (AFC with Ref));
   end With_Meta;

end Types.Maps;
