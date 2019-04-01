with Ada.Containers;
private with Ada.Finalization;

package Types.Symbols with Preelaborate is

   --  Like keys, symbols are immutable final nodes in the internal
   --  data structures. For them, reference counting is probably more
   --  efficient than garbage collecting.

   type Ptr is tagged private;

   function Constructor (Source : in String) return Ptr with Inline;

   function To_String (Item : in Ptr) return String with Inline;

   --  The hash value is made available because symbols have a high
   --  probability to end up as keys in an environment.
   function Hash (Item : in Ptr) return Ada.Containers.Hash_Type with Inline;

   --  The implementation ensures that a given content is only
   --  allocated once, so equality of pointers gives the same result
   --  than comparing the strings.

   type Symbol_Array is array (Positive range <>) of Ptr;
   Empty_Array : constant Symbol_Array;
   --  It is convenient to define this here because the default value
   --  for Ptr is invalid.

   --  Debug.
   procedure Check_Allocations with Inline;
   --  Does nothing if assertions are disabled.

private

   --  Only one instance is allocated with a given content.  This
   --  avoids many allocations and deallocations, since symbols are
   --  expected to be used many times.

   --  Tests seem to show that this solution is a few percents faster
   --  than Ada.Strings.Unbounded.

   --  As a side effect, some frequent string comparisons (with "def!"
   --  or "fn*" for example) will become a bit more efficient because
   --  comparing pointers is faster than comparing strings.

   --  It would be natural to store a Cursor from the global
   --  dictionnary into Ptr, but this actually reduces the speed,
   --  probably because it significantly increases the size of
   --  Mal_Type.

   type Rec;
   type Acc is access Rec;
   type Ptr is new Ada.Finalization.Controlled with record
      Ref : Acc := null;
   end record
     with Invariant => Ptr.Ref /= null;
   overriding procedure Adjust   (Object : in out Ptr) with Inline;
   overriding procedure Finalize (Object : in out Ptr) with Inline;
   --  Predefined equality is fine.
   pragma Finalize_Storage_Only (Ptr);

   Empty_Array : constant Symbol_Array
     := (1 .. 0 => (Ada.Finalization.Controlled with null));
   --  This will not trigger the invariant check because no element is
   --  ever actually instantiated.

end Types.Symbols;
