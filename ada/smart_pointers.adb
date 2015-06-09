with Ada.Unchecked_Deallocation;

package body Smart_Pointers is


   function New_Ptr (Base_Class : Base_Class_Accessor) return Smart_Pointer is
   begin
      return Smart_Pointer'
         (Ada.Finalization.Controlled with Pointer => Base_Class);
   end New_Ptr;


   function Deref (Ptr : Smart_Pointer) return Base_Class_Accessor is
   begin
      return Ptr.Pointer;
   end Deref;


   overriding procedure Adjust (Object : in out Smart_Pointer) is
   begin
      if Object.Pointer /= null then
         Object.Pointer.Ref_Count := Object.Pointer.Ref_Count + 1;
      end if;
   end Adjust;


   procedure Free is
     new Ada.Unchecked_Deallocation (Base_Class'Class, Base_Class_Accessor);

   overriding procedure Finalize (Object : in out Smart_Pointer) is
   begin
      if Object.Pointer /= null then
         if Object.Pointer.Ref_Count > 0 then
            Object.Pointer.Ref_Count := Object.Pointer.Ref_Count - 1;
            if Object.Pointer.Ref_Count = 0 then
               Free (Object.Pointer);
            end if;
         end if;
      end if;
   end Finalize;


   function Is_Null (Ptr : Smart_Pointer) return Boolean is
   begin
      return Ptr = Null_Smart_Pointer;
   end Is_Null;


end Smart_Pointers;
