package Garbage_Collected is

   --  A generic would not be convenient for lists. We want the
   --  extended type to be able to have a discriminant.

   --  However, we keep the dispatching in a single enumeration for
   --  efficiency and clarity of the source.

   type Instance is abstract tagged limited private;
   subtype Class is Instance'Class;
   type Link is access all Class;
   subtype Pointer is not null Link;

   procedure Keep_References (Object : in out Instance) is null with Inline;
   --  A dispatching call in Keep allows subclasses to override this
   --  in order to Keep each of the internal reference they maintain.

   --  The following methods have no reason to be overridden.

   procedure Keep (Object : in out Class) with Inline;
   --  Mark this object so that it is not deleted by next clean,
   --  then make a dispatching call to Keep_References.
   --  Does nothing if it has already been called for this object
   --  since startup or last Clean.

   procedure Register (Ref : in Pointer) with Inline;
   --  Each subclass defines its own allocation pool, but every call
   --  to new must be followed by a call to Register.

   procedure Clean;
   --  For each object for which Keep has not been called since
   --  startup or last clean, make a dispatching call to Finalize,
   --  then deallocate the memory for the object.

   --  Debug.
   procedure Check_Allocations;
   --  Does nothing if assertions are disabled.

private

   type Instance is abstract tagged limited record
      Kept : Boolean := False;
      Next : Link    := null;
   end record;

end Garbage_Collected;
