private with Ada.Finalization;
limited with Types;

package Lists is

   --  A pointer to an array of Mal_Type elements. It differs from
   --  Ada.Containers.Vectors because assignment give another pointer
   --  to the same storage and does not copy contents.

   type Ptr is tagged private;
   Empty_List : constant Ptr;           --  The default value.

   function Length (Source : in Ptr) return Natural
     with Inline;

   function Element (Container : in Ptr;
                     Index     : in Positive) return Types.Mal_Type
     with Inline, Pre => Index <= Container.Length;

   function Alloc (Length : in Natural) return Ptr
     with Inline;
   --  All elements are Nil, the default value for Mal_Type.

   function Alloc (Source : in Types.Mal_Type_Array) return Ptr
     with Inline;

   procedure Replace_Element (Source    : in Ptr;
                              Index     : in Positive;
                              New_Value : in Types.Mal_Type)
     with Inline, Pre => Index <= Source.Length;
   --  An assertion checks that Source is the only reference to its
   --  storage.

private

   type List_Record;
   type List_Access is access List_Record;
   type Ptr is new Ada.Finalization.Controlled with record
      Ref : List_Access := null;
   end record;
   overriding procedure Adjust (Object : in out Ptr) with Inline;
   overriding procedure Finalize (Object : in out Ptr) with Inline;
   overriding function "=" (Left, Right : in Ptr) return Boolean;

   Empty_List : constant Ptr := (Ada.Finalization.Controlled with Ref => null);

end Lists;
