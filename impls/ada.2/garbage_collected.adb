with Ada.Unchecked_Deallocation;

package body Garbage_Collected is

   procedure Free is new Ada.Unchecked_Deallocation (Class, Link);

   Top : Link := null;

   ----------------------------------------------------------------------

   procedure Clean is
      Current  : Link := Top;
      Previous : Link;
   begin
      while Current /= null and then not Current.all.Kept loop
         Previous := Current;
         Current := Current.all.Next;
         Free (Previous);
      end loop;
      Top := Current;
      while Current /= null loop
         if Current.all.Kept then
            Current.all.Kept := False;
            Previous := Current;
         else
            Previous.all.Next := Current.all.Next;
            Free (Current);
         end if;
         Current := Previous.all.Next;
      end loop;
   end Clean;

   procedure Keep (Object : in out Class) is
   begin
      if not Object.Kept then
         Object.Kept := True;
         Object.Keep_References;        --  dispatching
      end if;
   end Keep;

   procedure Check_Allocations is
   begin
      pragma Assert (Top = null);
   end Check_Allocations;

   procedure Register (Ref : in Pointer) is
   begin
      pragma Assert (Ref.all.Kept = False);
      pragma Assert (Ref.all.Next = null);
      Ref.all.Next := Top;
      Top := Ref;
   end Register;

end Garbage_Collected;
