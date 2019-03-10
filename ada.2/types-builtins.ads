private with Ada.Finalization;

limited with Types.Mal;

package Types.Builtins is

   --  Types.Mal.Builtin_Ptr is efficient and sufficient for most
   --  purposes, as counting references is a waste of time for native
   --  functions. The controlled type below is only useful when one
   --  has the silly idea to add metadata to a built-in.

   type Ptr is tagged private;

   function With_Meta (Builtin  : in Mal.Builtin_Ptr;
                       Metadata : in Mal.T) return Mal.T with Inline;
   function With_Meta (Item     : in Ptr;
                       Metadata : in Mal.T) return Mal.T with Inline;
   function Meta (Item : in Ptr) return Mal.T with Inline;
   function Builtin (Item : in Ptr) return Mal.Builtin_Ptr with Inline;

private

   type Rec;
   type Acc is access Rec;
   type Ptr is new Ada.Finalization.Controlled with record
      Ref : Acc := null;
   end record
     with Invariant => Ptr.Ref /= null;
   overriding procedure Adjust   (Object : in out Ptr) with Inline;
   overriding procedure Finalize (Object : in out Ptr) with Inline;
   pragma Finalize_Storage_Only (Ptr);

end Types.Builtins;
