with Ada.Containers;
private with Ada.Containers.Indefinite_Hashed_Sets;
private with Ada.Finalization;

package Strings is

   pragma Elaborate_Body;

   --  An abstraction similar to Ada.Strings.Unbounded, except that
   --  the type is immutable, and that only one instance is allocated
   --  with a given content.  This avoids many allocations and
   --  deallocations, since symbols and keywords are expected to be
   --  used many times.  Using this for all strings even if they are
   --  not used as keys in maps should not hurt.

   --  As a side effect, some frequent string comparisons (with "def!"
   --  or "fn*" for example) will become a bit more efficient because
   --  comparing pointers is faster than comparing strings.

   type Ptr is tagged private;
   Empty_String : constant Ptr;         --  The default value.

   function Alloc (Source : in String) return Ptr;

   function Deref (Source : in Ptr) return String
     with Inline;

   --  We make the hash value visible so that environments and maps do
   --  not need to recompute it.
   function Hash (Source : in Ptr) return Ada.Containers.Hash_Type
     with Inline;

private

   type Element_Type (Last : Positive) is record
      Data : String (1 .. Last);
      Hash : Ada.Containers.Hash_Type;
      Refs : Positive;
   end record;

   function Hash (Element : Element_Type) return Ada.Containers.Hash_Type
   is (Element.Hash)
     with Inline;

   function Equivalent_Elements (Left, Right : Element_Type) return Boolean
   is (Left.Data = Right.Data)
     with Inline;

   package Sets is new Ada.Containers.Indefinite_Hashed_Sets
     (Element_Type        => Element_Type,
      Hash                => Hash,
      Equivalent_Elements => Equivalent_Elements,
      "="                 => "=");

   type Ptr is new Ada.Finalization.Controlled with record
      Position : Sets.Cursor := Sets.No_Element;
   end record;
   overriding procedure Adjust (Object : in out Ptr) with Inline;
   overriding procedure Finalize (Object : in out Ptr) with Inline;
   --  Predefined equality is fine.

   Empty_String : constant Ptr
     := (Ada.Finalization.Controlled with Position => Sets.No_Element);

end Strings;
