with Ada.Containers.Hashed_Maps;
with Ada.Strings.Unbounded.Hash;
with Ada.Unchecked_Deallocation;

with Types.Lists;
with Types.Mal;

package body Types.Maps is

   subtype AFC is Ada.Finalization.Controlled;
   use type Ada.Containers.Count_Type;

   function Hash (Item : in Mal.T) return Ada.Containers.Hash_Type
     with Inline, Pre => Item.Kind in Kind_Keyword | Kind_String;

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

   ----------------------------------------------------------------------

   function "=" (Left, Right : in Ptr) return Boolean
   is (Left.Ref.all.Data = Right.Ref.all.Data);

   procedure Adjust (Object : in out Ptr) is
   begin
      Object.Ref.all.Refs := Object.Ref.all.Refs + 1;
   end Adjust;

   function Assoc (Args : in Mal.T_Array) return Mal.T is
      Binds : constant Natural := Args'Length / 2;
   begin
      if Args'Length mod 2 /= 1 then
         raise Argument_Error with "assoc: expects an odd argument count";
      elsif Args (Args'First).Kind /= Kind_Map then
         raise Argument_Error with "assoc: first argument must be a map";
      elsif (for some I in 1 .. Binds => Args (Args'First + 2 * I - 1).Kind
               not in Kind_Keyword | Kind_String)
      then
         raise Argument_Error with "assoc: keys must be strings or symbols";
      end if;
      declare
         Old : Rec renames Args (Args'First).Map.Ref.all;
         Ref : Acc;
      begin
         pragma Assert (0 < Old.Refs);
         if Old.Refs = 1 then
            Ref := Args (Args'First).Map.Ref;
            Old.Refs := 2;
            Old.Meta := Mal.Nil;
         else
            Ref := new Rec'(Data => Old.Data, others => <>);
         end if;
         for I in 1 .. Binds loop
            Ref.all.Data.Include (Key      => Args (Args'First + 2 * I - 1),
                                  New_Item => Args (Args'First + 2 * I));
         end loop;
         return (Kind_Map, (AFC with Ref));
      end;
   end Assoc;

   function Contains (Args : in Mal.T_Array) return Mal.T
   is (if Args'Length /= 2 then
          raise Argument_Error with "contains: expects 2 arguments"
       elsif Args (Args'First).Kind /= Kind_Map then
          raise Argument_Error with "contains: first arguement must be a map"
       else (Kind_Boolean,
             Args (Args'First).Map.Ref.all.Data.Contains (Args (Args'Last))));

   function Dissoc (Args : in Mal.T_Array) return Mal.T is
   begin
      if Args'Length = 0 then
         raise Argument_Error with "dissoc: expects at least 1 argument";
      elsif Args (Args'First).Kind /= Kind_Map then
         raise Argument_Error with "dissoc: first argument must be a map";
      elsif (for some I in Args'First + 1 .. Args'Last =>
               Args (I).Kind not in Kind_Keyword | Kind_String)
      then
         raise Argument_Error with "dissoc: keys must be strings or symbols";
      end if;
      declare
         Old : Rec renames Args (Args'First).Map.Ref.all;
         Ref : Acc;
      begin
         pragma Assert (0 < Old.Refs);
         if Old.Refs = 1 then
            Ref := Args (Args'First).Map.Ref;
            Old.Refs := 2;
            Old.Meta := Mal.Nil;
         else
            Ref := new Rec'(Data => Old.Data, others => <>);
         end if;
         for I in Args'First + 1 .. Args'Last loop
            Ref.all.Data.Exclude (Args (I));
         end loop;
         return (Kind_Map, (AFC with Ref));
      end;
   end Dissoc;

   procedure Finalize (Object : in out Ptr) is
   begin
      if Object.Ref /= null and then 0 < Object.Ref.all.Refs then
         Object.Ref.all.Refs := Object.Ref.all.Refs - 1;
         if 0 < Object.Ref.all.Refs then
            Object.Ref := null;
         else
            Free (Object.Ref);
         end if;
      end if;
   end Finalize;

   function Generic_Eval (Container : in Ptr;
                          Env       : in Env_Type)
                         return Mal.T is
      --  Copy the whole hash in order to avoid recomputing the hash
      --  for each key, even if it implies unneeded calls to adjust
      --  and finalize for Mal_Type values.
      Old : Rec renames Container.Ref.all;
      Ref : Acc;
   begin
      pragma Assert (0 < Old.Refs);
      if Old.Refs = 1 then
         Ref := Container.Ref;
         Old.Refs := 2;
         Old.Meta := Mal.Nil;
      else
         Ref := new Rec'(Data => Container.Ref.all.Data, others => <>);
      end if;
      --  Prepare a valid structure before running user code. In case
      --  an exception is raised, we want memory to be deallocated.
      return R : constant Mal.T := (Kind_Map, (AFC with Ref)) do
         for Position in Ref.all.Data.Iterate loop
            Ref.all.Data.Replace_Element (Position,
               Eval (HM.Element (Position), Env));
         end loop;
      end return;
   end Generic_Eval;

   function Get (Args : in Mal.T_Array) return Mal.T is
      Position : HM.Cursor;
   begin
      if Args'Length /= 2 then
         raise Argument_Error with "get: expects 2 arguments";
      elsif Args (Args'Last).Kind not in Kind_Keyword | Kind_String then
         raise Argument_Error with "get: key must be a keyword or string";
      end if;
      case Args (Args'First).Kind is
         when Kind_Nil =>
            return Mal.Nil;
         when Kind_Map =>
            Position
              := Args (Args'First).Map.Ref.all.Data.Find (Args (Args'Last));
            if HM.Has_Element (Position) then
               return HM.Element (Position);
            else
               return Mal.Nil;
            end if;
         when others =>
            raise Argument_Error with "get: first argument must be a map";
      end case;
   end Get;

   function Hash (Item : in Mal.T) return Ada.Containers.Hash_Type
   is (Ada.Strings.Unbounded.Hash (Item.S));

   function Hash_Map (Args : in Mal.T_Array) return Mal.T is
      Binds : constant Natural := Args'Length / 2;
      Ref   : Acc;
   begin
      if Args'Length mod 2 /= 0 then
         raise Argument_Error with "hash-map: expects an even argument count";
      elsif (for some I in 0 .. Binds - 1 => Args (Args'First + 2 * I).Kind
               not in Kind_Keyword | Kind_String)
      then
         raise Argument_Error with "hash-map: keys must be strings or symbols";
      end if;
      Ref := new Rec;
      Ref.all.Data.Reserve_Capacity (Ada.Containers.Count_Type (Binds));
      for I in 0 .. Binds - 1 loop
         Ref.all.Data.Include (Key      => Args (Args'First + 2 * I),
                               New_Item => Args (Args'First + 2 * I + 1));
      end loop;
      return (Kind_Map, (AFC with Ref));
   end Hash_Map;

   procedure Iterate (Container : in Ptr) is
   begin
      for Position in Container.Ref.all.Data.Iterate loop
         Process (HM.Key (Position), HM.Element (Position));
      end loop;
   end Iterate;

   function Keys (Args : in Mal.T_Array) return Mal.T is
   begin
      if Args'Length /= 1 then
         raise Argument_Error with "keys: expects 1 argument";
      elsif Args (Args'First).Kind /= Kind_Map then
         raise Argument_Error with "keys: first argument must a map";
      end if;
      declare
         A1 : HM.Map renames Args (Args'First).Map.Ref.all.Data;
         R  : Mal.T_Array (1 .. Natural (A1.Length));
         I  : Positive := 1;
      begin
         for Position in A1.Iterate loop
            R (I) := HM.Key (Position);
            I := I + 1;
         end loop;
         return Lists.List (R);
      end;
   end Keys;

   function Meta (Container : in Ptr) return Mal.T
   is (Container.Ref.all.Meta);

   function Vals (Args : in Mal.T_Array) return Mal.T is
   begin
      if Args'Length /= 1 then
         raise Argument_Error with "vals: expects 1 argument";
      elsif Args (Args'First).Kind /= Kind_Map then
         raise Argument_Error with "vals: first argument must be a map";
      end if;
      declare
         A1 : HM.Map renames Args (Args'First).Map.Ref.all.Data;
         R  : Mal.T_Array (1 .. Natural (A1.Length));
         I  : Positive := 1;
      begin
         for Element of A1 loop
            R (I) := Element;
            I := I + 1;
         end loop;
         return Lists.List (R);
      end;
   end Vals;

   function With_Meta (Data : in Ptr;
                       Meta : in Mal.T)
                      return Mal.T is
      Old : Rec renames Data.Ref.all;
      Ref : Acc;
   begin
      pragma Assert (0 < Old.Refs);
      if Old.Refs = 1 then
         Ref := Data.Ref;
         Old.Refs := 2;
         Old.Meta := Meta;
      else
         Ref := new Rec'(Data   => Old.Data,
                         Meta   => Meta,
                         others => <>);
      end if;
      return (Kind_Map, (AFC with Ref));
   end With_Meta;

end Types.Maps;
