package body Types.Builtins is

   function Builtin (Item : in Instance) return Mal.Builtin_Ptr
   is (Item.F_Builtin);

   procedure Keep_References (Object : in out Instance) is
   begin
      Mal.Keep (Object.F_Meta);
   end Keep_References;

   function Meta (Item : in Instance) return Mal.T
   is (Item.F_Meta);

   function With_Meta (Builtin  : in Mal.Builtin_Ptr;
                       Metadata : in Mal.T) return Mal.T
   is
      Ref : constant Mal.Builtin_With_Meta_Ptr
        := new Instance'(Garbage_Collected.Instance with
                         F_Builtin => Builtin,
                         F_Meta    => Metadata);
   begin
      Garbage_Collected.Register (Garbage_Collected.Pointer (Ref));
      return (Kind_Builtin_With_Meta, Ref);
   end With_Meta;

   function With_Meta (Item     : in Instance;
                       Metadata : in Mal.T) return Mal.T
   is (With_Meta (Item.Builtin, Metadata));

end Types.Builtins;
