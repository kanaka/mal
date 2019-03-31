private with Ada.Containers.Hashed_Maps;

with Garbage_Collected;
with Types.Mal;
with Types.Symbols;

package Envs is

   --  This package should be named Env, but Ada does not allow formal
   --  parameters to be named like a package dependency, and it seems
   --  that readability inside Eval is more important.

   type Instance (<>) is new Garbage_Collected.Instance with private;
   type Ptr is access Instance;

   No_Binds : Types.Symbols.Symbol_Array renames Types.Symbols.Empty_Array;
   No_Exprs : constant Types.Mal.T_Array := (1 .. 0 => Types.Mal.Nil);

   function New_Env (Outer : in Ptr                        := null;
                     Binds : in Types.Symbols.Symbol_Array := No_Binds;
                     Exprs : in Types.Mal.T_Array          := No_Exprs)
                    return Ptr;

   function Get (Env : in Instance;
                 Key : in Types.Symbols.Ptr) return Types.Mal.T;

   procedure Set (Env      : in out Instance;
                  Key      : in     Types.Symbols.Ptr;
                  New_Item : in     Types.Mal.T) with Inline;

   --  Debug.
   procedure Dump_Stack (Env : in Instance);

private

   package HM is new Ada.Containers.Hashed_Maps
     (Key_Type        => Types.Symbols.Ptr,
      Element_Type    => Types.Mal.T,
      Hash            => Types.Symbols.Hash,
      Equivalent_Keys => Types.Symbols."=",
      "="             => Types.Mal."=");

   type Instance is new Garbage_Collected.Instance with record
      Outer : Ptr;
      Data  : HM.Map;
   end record;
   overriding procedure Keep_References (Object : in out Instance) with Inline;

end Envs;
