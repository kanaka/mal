with Ada.Containers;

with Garbage_Collected;

package Types.Strings is

   ------------------------------------
   --  Keywords, Strings and Symbols --
   ------------------------------------

   --  Tests seem to show that manual garbage collection is faster
   --  than reference counting in Ada.Strings.Unbounded, probably
   --  because we know that the values will never change.

   --  Also, maintaining a global structure in order to avoid similar
   --  symbol allocations does not seem to improve performances.

   type Instance (<>) is abstract new Garbage_Collected.Instance with private;

   function Alloc (Data : in String) return String_Ptr
     with Inline;

   function "=" (Left  : in Instance;
                 Right : in String) return Boolean
     with Inline;

   --  This kind of accessor is more efficient than a function
   --  returning an array.
   procedure Query_Element
     (Container : in Instance;
      Process   : not null access procedure (Element : in String));

   --  These methods could be implemented with Query_Element,
   --  but we want to optimize Envs.Get.
   function Hash (Item : in String_Ptr) return Ada.Containers.Hash_Type
     with Inline;
   function Same_Contents (Left, Right : in String_Ptr) return Boolean
     with Inline;

   --  When readability is more important than copying a string.
   function To_String (Container : in Instance) return String with Inline;

private

   type Instance (Last : Natural) is new Garbage_Collected.Instance with record
      Data : String (1 .. Last);
   end record;

end Types.Strings;
