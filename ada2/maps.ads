private with Ada.Finalization;
with Lists;
limited with Types;

package Maps is

   --  A pointer to an Ada.Containers.Hashed_Maps.Map of
   --  Types.Mal_Type. Keys must be Strings or Keywords.  We can
   --  probably not state this with a limited with, so this will
   --  become an assertion.

   type Ptr is tagged private;
   --  The default value is empty.

   function Length (Container : in Ptr) return Natural
     with Inline;

   function Hash_Map (Pairs : in Types.Mal_Type_Array) return Ptr;

   function Assoc (Container : in Ptr;
                   Pairs     : in Types.Mal_Type_Array) return Ptr;

   function Dissoc (Source : in Ptr;
                    Keys   : in Types.Mal_Type_Array) return Ptr;

   function Map (Container : in Ptr;
                 F         : not null access function (X : in Types.Mal_Type)
                                                      return Types.Mal_Type)
                return Ptr;

   procedure Iterate
     (Container : in Ptr;
      Process   : not null access procedure (Key     : in Types.Mal_Type;
                                             Element : in Types.Mal_Type))
     with Inline;

   function Contains (Container : in Ptr;
                      Key       : in Types.Mal_Type) return Boolean
     with Inline;

   function Get (Container : in Ptr;
                 Key       : in Types.Mal_Type) return Types.Mal_Type
     with Inline;
   Unknown_Key : exception;

private

   type Map_Record;
   type Map_Access is access Map_Record;
   type Ptr is new Ada.Finalization.Controlled with record
      Ref : Map_Access := null;
   end record
     with Invariant => Ptr.Ref /= null;
   overriding procedure Initialize (Object : in out Ptr) with Inline;
   overriding procedure Adjust (Object : in out Ptr) with Inline;
   overriding procedure Finalize (Object : in out Ptr) with Inline;
   overriding function "=" (Left, Right : in Ptr) return Boolean with Inline;

end Maps;
