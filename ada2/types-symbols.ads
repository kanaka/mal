with Ada.Containers;
private with Ada.Finalization;

package Types.Symbols with Preelaborate is

   type Ptr is tagged private;
   --  A wrapper for a pointer counting references.

   --  The default value is invalid, new variables must be assigned
   --  immediately (a hidden discriminant would prevent this type to
   --  become a field inside Types.Mal.T, so we check this with a
   --  private invariant a fallback, an invariant in the private part
   --  checks that any created object is affected before use.

   --  Assignment give another reference to the same storage.

   function Constructor (Source : in String) return Ptr with Inline;
   --  The only way to assign a valid value.

   function To_String (Item : in Ptr) return String with Inline;

   --  The hash value is made available because symbols have a high
   --  probability to end up as keys in an environment.
   function Hash (Item : in Ptr) return Ada.Containers.Hash_Type with Inline;

   --  Equality compares the contents.

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

   --  See README for the implementation of reference counting.

   type Rec;
   type Acc is access Rec;
   type Ptr is new Ada.Finalization.Controlled with record
      Ref : Acc := null;
   end record
     with Invariant => Ref /= null;
   overriding procedure Adjust   (Object : in out Ptr) with Inline;
   overriding procedure Finalize (Object : in out Ptr) with Inline;
   --  Predefined equality is fine.
   pragma Finalize_Storage_Only (Ptr);

end Types.Symbols;
