private with Ada.Finalization;

limited with Types.Mal;

package Types.Atoms is

   type Ptr is private;

   --  Built-in functions.
   function Atom  (Args : in Mal.T_Array) return Mal.T;
   function Deref (Args : in Mal.T_Array) return Mal.T;
   function Reset (Args : in Mal.T_Array) return Mal.T;
   function Swap  (Args : in Mal.T_Array) return Mal.T;

   --  Helper for print.
   function Deref (Item : in Ptr) return Mal.T with Inline;

   --  Debug.
   procedure Check_Allocations;

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

end Types.Atoms;
