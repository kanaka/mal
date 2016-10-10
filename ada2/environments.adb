with Ada.Containers.Hashed_Maps; use type Ada.Containers.Count_Type;
with Ada.Unchecked_Deallocation;
with Atoms;
with Names;
with Strings; use type Strings.Ptr;
with Types; use type Types.Kind_Type;

package body Environments is

   --  There must be a reference level so that functions may keep
   --  track of their initial environment, and another one for
   --  reallocations.

   package Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Strings.Ptr,
      Element_Type    => Types.Mal_Type,
      Hash            => Strings.Hash,
      Equivalent_Keys => Strings."=",
      "="             => Types."=");

   type Env_Record is limited record
      Data  : Maps.Map;
      Outer : Env_Access;
      Refs  : Positive;
   end record;

   procedure Free is new Ada.Unchecked_Deallocation (Object => Env_Record,
                                                     Name   => Env_Access);

   ----------------------------------------------------------------------

   procedure Adjust (Object : in out Ptr) is
   begin
      Object.Ref.all.Refs := Object.Ref.all.Refs + 1;
   end Adjust;

   function Alloc return Ptr
   is (Ada.Finalization.Controlled with
         Ref => new Env_Record'(Data  => Maps.Empty_Map,
                                Outer => null,
                                Refs  => 1));

   function Alloc (Outer : in Ptr) return Ptr is
   begin
      Outer.Ref.all.Refs := Outer.Ref.all.Refs + 1;
      return (Ada.Finalization.Controlled with
                Ref => new Env_Record'(Data  => Maps.Empty_Map,
                                       Outer => Outer.Ref,
                                       Refs  => 1));
   end Alloc;

   procedure Finalize (Object : in out Ptr)
   is
      Ref  : Env_Access;
      Refs : Positive;
   begin
      if Object.Ref /= null then
         Ref := Object.Ref;
         Object.Ref := null;
         loop
            Refs := Ref.all.Refs;
            if 1 < Refs then
               Ref.all.Refs := Refs - 1;
               exit;
            end if;
            declare
               Tmp : Env_Access := Ref;
            begin
               Ref := Ref.all.Outer;
               Free (Tmp);
            end;
            exit when Ref = null;
         end loop;
      end if;
   end Finalize;

   function Get (Container : in Ptr;
                 Key       : in Strings.Ptr) return Types.Mal_Type
   is
      Ref      : Env_Access := Container.Ref;
      Position : Maps.Cursor;
   begin
      loop
         Position := Ref.all.Data.Find (Key);
         if Maps.Has_Element (Position) then
            return Ref.all.Data (Position);
         end if;
         Ref := Ref.all.Outer;
         exit when Ref = null;
      end loop;
      raise Unknown_Key with "'" & Key.Deref & "' not found";
   end Get;

   procedure Increase_Capacity (Container : in Ptr;
                                Capacity  : in Natural)
   is
      New_Capacity : constant Ada.Containers.Count_Type
        := Container.Ref.all.Data.Length
        + Ada.Containers.Count_Type (Capacity);
   begin
      if Container.Ref.all.Data.Capacity < New_Capacity then
         Container.Ref.all.Data.Reserve_Capacity (New_Capacity);
      end if;
   end Increase_Capacity;

   procedure Replace_With_Subenv (Item : in out Ptr) is
   begin
      if 1 < Item.Ref.all.Refs then
         Item.Ref := new Env_Record'(Data  => Maps.Empty_Map,
                                     Outer => Item.Ref,
                                     Refs  => 1);
      end if;
   end Replace_With_Subenv;

   procedure Set (Container : in Ptr;
                  Key       : in Strings.Ptr;
                  New_Item  : in Types.Mal_Type) is
   begin
      Container.Ref.all.Data.Include (Key, New_Item);
   end Set;

   procedure Set_Binds (Container : in Ptr;
                        Formals   : in Lists.Ptr;
                        Actuals   : in Types.Mal_Type_Array)
   is
      --  The assertions should be a precondition, but cannot be
      --  expressed with a "limited with" view on Types.
   begin
      if Formals.Length <= 1
        or else Formals.Element (Formals.Length - 1).S /= Names.Ampersand
      then
         pragma Assert (Formals.Length = Actuals'Length);
         pragma Assert (for all I in 1 .. Formals.Length =>
                          Formals.Element (I).Kind = Types.Kind_Symbol
                          and then Formals.Element (I).S /= Names.Ampersand);
         Increase_Capacity (Container, Formals.Length);
         for I in 1 .. Formals.Length loop
            Container.Ref.all.Data.Include (Formals.Element (I).S,
                                            Actuals (Actuals'First + I - 1));
         end loop;
      else
         declare
            Len  : constant Natural := Formals.Length - 2;
         begin
            pragma Assert (Len <= Actuals'Length);
            pragma Assert (for all I in 1 ..  Len =>
                           Formals.Element (I).Kind = Types.Kind_Symbol
                           and then Formals.Element (I).S /= Names.Ampersand);
            pragma Assert (Formals.Element (Len + 1).Kind = Types.Kind_Symbol);
            pragma Assert (Formals.Element (Len + 1).S = Names.Ampersand);
            pragma Assert (Formals.Element (Len + 2).Kind = Types.Kind_Symbol);
            pragma Assert (Formals.Element (Len + 2).S /= Names.Ampersand);
            Increase_Capacity (Container, Len + 1);
            for I in 1 .. Len loop
               Container.Ref.all.Data.Include
                 (Formals.Element (I).S, Actuals (Actuals'First + I - 1));
            end loop;
            Container.Ref.all.Data.Include
              (Formals.Element (Formals.Length).S,
               (Types.Kind_List, Atoms.No_Element,
                Lists.Alloc (Actuals (Actuals'First + Len .. Actuals'Last))));
         end;
      end if;
   end Set_Binds;

end Environments;
