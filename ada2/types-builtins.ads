private with Ada.Finalization;

limited with Types.Mal;

package Types.Builtins is

   type Ptr is access function (Args : in Mal.T_Array) return Mal.T;
   --  This access type is efficient and sufficient for most purposes,
   --  as counting references is a waste of time for native functions,
   --  which are often used as atomic elements. The controlled type
   --  below is only useful when one has the silly idea to add
   --  metadata to a built-in.

   type Ptr_With_Meta is tagged private;
   --  A wrapper for a pointer counting references.

   --  The default value is invalid, new variables must be assigned
   --  immediately (a hidden discriminant would prevent this type to
   --  become a field inside Types.Mal.T, so we check this with a
   --  private invariant a fallback, an invariant in the private part
   --  checks that any created object is affected before use.

   --  Assignment give another reference to the same storage.

   function With_Meta (Data : in Ptr;
                       Meta : in Mal.T) return Mal.T with Inline;
   function With_Meta (Data : in Ptr_With_Meta;
                       Meta : in Mal.T) return Mal.T with Inline;
   function Meta (Item : in Ptr_With_Meta) return Mal.T with Inline;
   function Data (Item : in Ptr_With_Meta) return Ptr with Inline;

private

   --  See README for the implementation of reference counting.

   type Rec;
   type Acc is access Rec;
   type Ptr_With_Meta is new Ada.Finalization.Controlled with record
      Ref : Acc := null;
   end record
     with Invariant => Ref /= null;
   overriding procedure Adjust   (Object : in out Ptr_With_Meta) with Inline;
   overriding procedure Finalize (Object : in out Ptr_With_Meta) with Inline;
   pragma Finalize_Storage_Only (Ptr_With_Meta);

end Types.Builtins;
