with Ada.Unchecked_Deallocation;

with Types.Mal;

package body Types.Builtins is

   type Rec is limited record
      Builtin : Mal.Builtin_Ptr;
      Refs    : Natural;
      Meta    : Mal.T;
   end record;

   procedure Free is new Ada.Unchecked_Deallocation (Rec, Acc);
   Allocations : Natural := 0;

   ----------------------------------------------------------------------

   procedure Adjust (Object : in out Ptr) is
   begin
      Object.Ref.all.Refs := @ + 1;
   end Adjust;

   function Builtin (Item : in Ptr) return Mal.Builtin_Ptr
   is (Item.Ref.all.Builtin);

   procedure Check_Allocations is
   begin
      pragma Assert (Allocations = 0);
   end Check_Allocations;

   procedure Finalize (Object : in out Ptr) is
   begin
      if Object.Ref /= null and then 0 < Object.Ref.all.Refs then
         Object.Ref.all.Refs := @ - 1;
         if 0 < Object.Ref.all.Refs then
            Object.Ref := null;
         else
            Allocations := Allocations - 1;
            Free (Object.Ref);
         end if;
      end if;
   end Finalize;

   function Meta (Item : in Ptr) return Mal.T
   is (Item.Ref.all.Meta);

   function With_Meta (Builtin  : in Mal.Builtin_Ptr;
                       Metadata : in Mal.T) return Mal.T is
   begin
      Allocations := Allocations + 1;
      return (Kind_Builtin_With_Meta,
              (Ada.Finalization.Controlled with new Rec'(Builtin => Builtin,
                                                         Meta    => Metadata,
                                                         Refs    => 1)));
   end With_Meta;

   function With_Meta (Item     : in Ptr;
                       Metadata : in Mal.T) return Mal.T
     --  Do not try to reuse the memory. We can hope that this kind of
     --  nonsense will be rare.
   is (With_Meta (Item.Ref.all.Builtin, Metadata));

end Types.Builtins;
