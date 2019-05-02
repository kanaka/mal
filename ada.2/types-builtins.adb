package body Types.Builtins is

   function Builtin (Item : in Instance) return Builtin_Ptr
   is (Item.F_Builtin);

   procedure Keep_References (Object : in out Instance) is
   begin
      Keep (Object.F_Meta);
   end Keep_References;

   function Meta (Item : in Instance) return T
   is (Item.F_Meta);

   function With_Meta (Builtin  : in Builtin_Ptr;
                       Metadata : in T) return T
   is
      --  Builtin is not null and requires an immediate initialization.
      Ref : constant Builtin_With_Meta_Ptr
        := new Instance'(Garbage_Collected.Instance with
                         F_Builtin => Builtin,
                         F_Meta    => Metadata);
   begin
      Garbage_Collected.Register (Garbage_Collected.Pointer (Ref));
      return (Kind_Builtin_With_Meta, Ref);
   end With_Meta;

   function With_Meta (Builtin  : in Instance;
                       Metadata : in T) return T
   is (With_Meta (Builtin.F_Builtin, Metadata));

end Types.Builtins;
