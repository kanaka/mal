private with Ada.Finalization;

limited with Types.Mal;

package Types.Atoms is

   type Ptr is private;
   --  A wrapper for a pointer counting references.

   --  The default value is invalid, new variables must be assigned
   --  immediately (a hidden discriminant would prevent this type to
   --  become a field inside Types.Mal.T, so we check this with a
   --  private invariant a fallback, an invariant in the private part
   --  checks that any created object is affected before use.

   --  Assignment give another reference to the same storage.

   --  Built-in functions.
   function Atom  (Args : in Mal.T_Array) return Mal.T;
   function Deref (Args : in Mal.T_Array) return Mal.T;
   function Reset (Args : in Mal.T_Array) return Mal.T;

private

   type Rec;
   type Acc is access Rec;
   type Ptr is new Ada.Finalization.Controlled with record
      Ref : Acc := null;
   end record
     with Invariant => Ref /= null;
   overriding procedure Adjust   (Object : in out Ptr) with Inline;
   overriding procedure Finalize (Object : in out Ptr) with Inline;
   pragma Finalize_Storage_Only (Ptr);

end Types.Atoms;
