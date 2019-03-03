private with Ada.Finalization;

limited with Environments;
limited with Types.Lists;
limited with Types.Mal;

package Types.Functions is

   type Ptr is tagged private;
   --  A wrapper for a pointer counting references.

   --  The default value is invalid, new variables must be assigned
   --  immediately (a hidden discriminant would prevent this type to
   --  become a field inside Types.Mal.T, so we check this with a
   --  private invariant a fallback, an invariant in the private part
   --  checks that any created object is affected before use.

   --  Assignment give another reference to the same storage.

   function New_Function (Formals     : in Lists.Ptr;
                          Expression  : in Mal.T;
                          Environment : in Environments.Closure_Ptr)
                         return Mal.T
     with Inline;

   --  Equivalent to a sequence of Set with the formal parameters and
   --  Args elements, except for the handling of "&".
   --  May raise Argument_Count.
   --  For functions.
   procedure Set_Binds (Item : in Ptr;
                        Env  : in Environments.Ptr;
                        Args : in Mal.T_Array);

   function New_Macro (Item : in Ptr) return Mal.T with Inline;
   --  Set_Binds for macros.
   --  It skips the first element of Args.
   procedure Set_Binds (Item : in Ptr;
                        Env  : in Environments.Ptr;
                        Args : in Lists.Ptr);

   --  Used when printing, or applying with specific requirements,
   --  like allowing tail call optimization or macros.
   function Formals    (Item : in Ptr) return Lists.Ptr with Inline;
   function Expression (Item : in Ptr) return Mal.T with Inline;
   function Closure    (Item : in Ptr) return Environments.Closure_Ptr
     with Inline;

   function Meta (Item : in Ptr) return Mal.T with Inline;
   function With_Meta (Data     : in Ptr;
                       Metadata : in Mal.T)
                      return Mal.T with Inline;

private

   --  See README for the implementation of reference counting.

   type Rec;
   type Acc is access Rec;
   type Ptr is new Ada.Finalization.Controlled with record
      Ref : Acc := null;
   end record
     with Invariant => Ref /= null;
   overriding procedure Adjust   (Object : in out Ptr) with Inline;
   overriding procedure Finalize (Object : in out Ptr) with Inline;
   pragma Finalize_Storage_Only (Ptr);

end Types.Functions;
