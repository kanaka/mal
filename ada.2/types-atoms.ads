with Garbage_Collected;
with Types.Mal;

package Types.Atoms is

   type Instance (<>) is new Garbage_Collected.Instance with private;

   --  Built-in functions.
   function Atom  (Args : in Mal.T_Array) return Mal.T;
   function Deref (Args : in Mal.T_Array) return Mal.T;
   function Reset (Args : in Mal.T_Array) return Mal.T;
   function Swap  (Args : in Mal.T_Array) return Mal.T;

   --  Helper for print.
   function Deref (Item : in Instance) return Mal.T with Inline;

private

   type Instance is new Garbage_Collected.Instance with record
      Data : Mal.T;
   end record;
   overriding procedure Keep_References (Object : in out Instance) with Inline;

end Types.Atoms;
