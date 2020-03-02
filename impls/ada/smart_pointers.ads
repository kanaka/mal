with Ada.Finalization;

package Smart_Pointers is

   -- Classes we want to track derrive from Base Class.
   type Base_Class is abstract tagged private;

   type Base_Class_Accessor is access Base_Class'Class;


   type Smart_Pointer is private;

   function New_Ptr (Base_Class : Base_Class_Accessor) return Smart_Pointer;

   function Deref (Ptr : Smart_Pointer) return Base_Class_Accessor;

   Null_Smart_Pointer : constant Smart_Pointer;

   function Is_Null (Ptr : Smart_Pointer) return Boolean;

private

   type Base_Class is abstract tagged record
      Ref_Count : Natural := 1;
   end record;


   type Smart_Pointer is new Ada.Finalization.Controlled with record
      Pointer : Base_Class_Accessor;
   end record;

   overriding procedure Adjust (Object : in out Smart_Pointer);

   overriding procedure Finalize (Object : in out Smart_Pointer);

   Null_Smart_Pointer : constant Smart_Pointer :=
      (Ada.Finalization.Controlled with Pointer => null);

end Smart_Pointers;
