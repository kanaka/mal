private with Ada.Containers.Hashed_Maps;

with Garbage_Collected;
with Types.Strings;

package Envs is

   --  This package should be named Env, but Ada does not allow formal
   --  parameters to be named like a package dependency, and it seems
   --  that readability inside Eval is more important.

   type Instance (<>) is abstract new Garbage_Collected.Instance with private;
   type Link is access Instance;
   subtype Ptr is not null Link;

   function New_Env (Outer : in Link := null) return Ptr with Inline;
   --  Set_Binds is provided as distinct subprograms because we some
   --  time spare the creation of a subenvironment.

   procedure Set_Binds (Env   : in out Instance;
                        Binds : in     Types.T_Array;
                        Exprs : in     Types.T_Array);
   --  Equivalent to successive calls to Set, except that if Binds
   --  ends with "&" followed by a symbol, the trailing symbol
   --  receives all remaining values as a list.

   function Get (Env : in Instance;
                 Key : in Types.String_Ptr) return Types.T;

   procedure Set (Env      : in out Instance;
                  Key      : in     Types.T;
                  New_Item : in     Types.T) with Inline;
   --  Raises an exception if Key is not a symbol.

   --  Debug.
   procedure Dump_Stack (Env : in Instance);

private

   package HM is new Ada.Containers.Hashed_Maps
     (Key_Type        => Types.String_Ptr,
      Element_Type    => Types.T,
      Hash            => Types.Strings.Hash,
      Equivalent_Keys => Types.Strings.Same_Contents,
      "="             => Types."=");

   --  It may be tempting to subclass Types.Map, but this would not
   --  simplify the code much. And adding metadata to a structure that
   --  is allocated very often has a cost.

   type Instance is new Garbage_Collected.Instance with record
      Outer : Link;
      Data  : HM.Map;
   end record;
   overriding procedure Keep_References (Object : in out Instance) with Inline;

end Envs;
