pragma Warnings (Off, "unit ""Types.Sequences"" is not referenced");
with Types.Sequences;
pragma Warnings (On, "unit ""Types.Sequences"" is not referenced");

package body Types.Macros is

   function Ast (Item : in Instance) return T
   is (Item.F_Ast);

   procedure Keep_References (Object : in out Instance) is
   begin
      Keep (Object.F_Ast);
      Object.F_Params.all.Keep;
   end Keep_References;

   function New_Macro (Func : in Fns.Instance) return T is
      --  Params is not null and requires an immediate initialization.
      Ref : constant Macro_Ptr := new Instance'
        (Garbage_Collected.Instance with Func.Ast, Func.Params);
   begin
      Garbage_Collected.Register (Garbage_Collected.Pointer (Ref));
      return (Kind_Macro, Ref);
   end New_Macro;

   function Params (Item : in Instance) return Sequence_Ptr
   is (Item.F_Params);

end Types.Macros;
