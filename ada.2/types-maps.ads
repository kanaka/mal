private with Ada.Containers.Hashed_Maps;

with Garbage_Collected;

package Types.Maps is

   --  All function receiving a key check that its kind is keyword or
   --  string.

   type Instance (<>) is abstract new Garbage_Collected.Instance with private;

   --  Built-in functions.
   function Assoc    (Args : in T_Array) return T;
   function Contains (Args : in T_Array) return T;
   function Dissoc   (Args : in T_Array) return T;
   function Get      (Args : in T_Array) return T;
   function Hash_Map (Args : in T_Array) return T;
   function Keys     (Args : in T_Array) return T;
   function Vals     (Args : in T_Array) return T;

   function "=" (Left, Right : in Instance) return Boolean with Inline;

   --  Used to print each element of a map.
   type Cursor (<>) is limited private;
   function Has_Element (Position : in Cursor) return Boolean with Inline;
   function Key (Position  : in Cursor) return T with Inline;
   function Element (Position  : in Cursor) return T with Inline;
   function First (Container : in Instance) return Cursor with Inline;
   procedure Next (Position : in out Cursor) with Inline;

   --  Used to evaluate each element of a map.
   function New_Map (Source : in Instance) return T with Inline;
   procedure Replace_Element (Container : in out Instance;
                              Position  : in     Cursor;
                              New_Item  : in     T) with Inline;

   function Meta (Container : in Instance) return T with Inline;
   function With_Meta (Container : in Instance;
                       Metadata  : in T) return T with Inline;

private

   function Hash (Item : in T) return Ada.Containers.Hash_Type with Inline;
   --  This function also checks the kind of the key, and raise an
   --  error in case of problem.

   package HM is new Ada.Containers.Hashed_Maps (Key_Type        => T,
                                                 Element_Type    => T,
                                                 Hash            => Hash,
                                                 Equivalent_Keys => "=",
                                                 "="             => "=");

   type Instance is new Garbage_Collected.Instance with record
      Data   : HM.Map;
      F_Meta : T;
   end record;

   overriding procedure Keep_References (Object : in out Instance) with Inline;

   type Cursor is new HM.Cursor;

end Types.Maps;
