with Ada.Unchecked_Deallocation;

with Types.Mal;

package body Types.Atoms is

   type Rec is limited record
      Refs : Natural;
      Data : Mal.T;
   end record;

   procedure Free is new Ada.Unchecked_Deallocation (Rec, Acc);

   ----------------------------------------------------------------------

   procedure Adjust (Object : in out Ptr) is
   begin
      Object.Ref.all.Refs := Object.Ref.all.Refs + 1;
   end Adjust;

   function Atom (Args : in Mal.T_Array) return Mal.T
   is (if Args'Length /= 1 then
          raise Argument_Error with "atom: expects 1 argument"
       else
          (Kind => Kind_Atom,
           Atom => (Ada.Finalization.Controlled with
                      Ref => new Rec'(Data => Args (Args'First),
                                      Refs => 1))));

   function Deref (Args : in Mal.T_Array) return Mal.T
   is (if Args'Length /= 1 then
          raise Argument_Error with "deref: expects 1 argument"
       elsif Args (Args'First).Kind /= Kind_Atom then
          raise Argument_Error with "deref: expects an atom"
       else
          (Args (Args'First).Atom.Ref.all.Data));

   procedure Finalize (Object : in out Ptr) is
   begin
      if Object.Ref /= null and then 0 < Object.Ref.all.Refs then
         Object.Ref.all.Refs := Object.Ref.all.Refs - 1;
         if 0 < Object.Ref.all.Refs then
            Object.Ref := null;
         else
            Free (Object.Ref);
         end if;
      end if;
   end Finalize;

   function Reset (Args : in Mal.T_Array) return Mal.T is
   begin
      if Args'Length /= 2 then
         raise Argument_Error with "reset: expects 2 arguments";
      elsif Args (Args'First).Kind /= Kind_Atom then
         raise Argument_Error with "reset: first argument must be an atom";
      else
         Args (Args'First).Atom.Ref.all.Data := Args (Args'Last);
         return Args (Args'Last);
      end if;
   end Reset;

end Types.Atoms;
