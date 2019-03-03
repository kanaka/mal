private with Ada.Finalization;

limited with Types.Mal;

package Types.Lists is

   type Ptr is tagged private;
   --  A wrapper for a pointer counting references.

   --  The default value is invalid, new variables must be assigned
   --  immediately (a hidden discriminant would prevent this type to
   --  become a field inside Types.Mal.T, so we check this with a
   --  private invariant a fallback, an invariant in the private part
   --  checks that any created object is affected before use.

   --  Assignment give another reference to the same storage.

   --  Built-in functions.
   function Concat   (Args : in Mal.T_Array) return Mal.T;
   function Conj     (Args : in Mal.T_Array) return Mal.T;
   function Cons     (Args : in Mal.T_Array) return Mal.T;
   function Count    (Args : in Mal.T_Array) return Mal.T;
   function First    (Args : in Mal.T_Array) return Mal.T;
   function Is_Empty (Args : in Mal.T_Array) return Mal.T;
   function List     (Args : in Mal.T_Array) return Mal.T;
   function Nth      (Args : in Mal.T_Array) return Mal.T;
   function Rest     (Args : in Mal.T_Array) return Mal.T;
   function Vector   (Args : in Mal.T_Array) return Mal.T;

   function Length (Source : in Ptr) return Natural with Inline;

   function Element (Container : in Ptr;
                     Index     : in Positive) return Mal.T
     with Inline;
   Index_Error : exception;

   function "&" (Left  : in Mal.T_Array;
                 Right : in Ptr) return Mal.T_Array;
   --  Used to implement Core.Apply.

   --  Used to evaluate each element of a list/vector.
   --  Eval is generic because units cannot depend on each other.
   generic
      type Env_Type (<>) is limited private;
      with function Eval (Ast : in Mal.T;
                          Env : in Env_Type)
                         return Mal.T;
   function Generic_Eval (Container : in Ptr;
                          Env       : in Env_Type)
                         return Ptr;

   --  Used to spare an intermediate copy for & in macro arguments.
   function Slice (Item  : in Ptr;
                   Start : in Positive)
                  return Mal.T;

   function Meta (Item : in Ptr) return Mal.T with Inline;
   function With_Meta (Data     : in Ptr;
                       Metadata : in Mal.T)
                      return Ptr;

private

   --  It is tempting to use null to represent an empty list, but the
   --  performance is not improved much, and the code is more complex.
   --  In addition, the empty list may want to carry metadata.

   --  Similarly, always providing a default value like a pointer to a
   --  static empty list would not gain much, and probably hide some
   --  bugs.

   type Rec;
   type Acc is access Rec;
   type Ptr is new Ada.Finalization.Controlled with record
      Ref : Acc := null;
   end record
     with Invariant => Ref /= null;
   overriding procedure Adjust   (Object : in out Ptr) with Inline;
   overriding procedure Finalize (Object : in out Ptr) with Inline;
   overriding function "=" (Left, Right : in Ptr) return Boolean;
   pragma Finalize_Storage_Only (Ptr);

end Types.Lists;
