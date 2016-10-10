private with Ada.Finalization;
with Lists;
with Strings;
limited with Types;

package Environments is

   type Ptr is tagged private;
   --  Any variable must be assigned immediately with one of the two
   --  following functions.
   function Alloc return Ptr
     with Inline;
   function Alloc (Outer : in Ptr) return Ptr
     with Inline;
   --  A hidden invariant ensures this when assertions are enabled.

   procedure Increase_Capacity (Container : in Ptr;
                                Capacity  : in Natural)
     with Inline;

   procedure Replace_With_Subenv (Item : in out Ptr)
     with Inline;
   --  Equivalent to Item := Alloc (Outer => Item, Capacity), but
   --  faster when Item was the last reference to its environment, as
   --  the storage and maps are then reused.

   procedure Set (Container : in Ptr;
                  Key       : in Strings.Ptr;
                  New_Item  : in Types.Mal_Type)
     with Inline;

   procedure Set_Binds (Container : in Ptr;
                        Formals   : in Lists.Ptr;
                        Actuals   : in Types.Mal_Type_Array);

   function Get (Container : in Ptr;
                 Key       : in Strings.Ptr) return Types.Mal_Type;
   Unknown_Key : exception;

   --  procedure Dump;

private

   type Env_Record;
   type Env_Access is access Env_Record;
   type Ptr is new Ada.Finalization.Controlled with record
      Ref : Env_Access := null;
   end record
     with Invariant => Ptr.Ref /= null;
   overriding procedure Adjust (Object : in out Ptr) with Inline;
   overriding procedure Finalize (Object : in out Ptr);
   --  Predefined equality is fine.

end Environments;
