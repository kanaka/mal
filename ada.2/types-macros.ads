with Garbage_Collected;
with Types.Fns;

package Types.Macros is

   type Instance (<>) is abstract new Garbage_Collected.Instance with private;

   function New_Macro (Func : in Fns.Instance) return T with Inline;

   function Ast (Item : in Instance) return T with Inline;
   function Params (Item : in Instance) return Sequence_Ptr with Inline;

private

   type Instance is new Garbage_Collected.Instance with record
      F_Ast    : T;
      F_Params : Sequence_Ptr;
   end record;

   overriding procedure Keep_References (Object : in out Instance) with Inline;

end Types.Macros;
