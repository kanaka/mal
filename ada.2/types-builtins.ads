with Garbage_Collected;

package Types.Builtins is

   --  Types.Mal.Builtin_Ptr is efficient and sufficient for most
   --  purposes, as native function need no deallocation.  The type
   --  below is only useful to add metadata to a built-in.

   type Instance (<>) is abstract new Garbage_Collected.Instance with private;

   function With_Meta (Builtin  : in Builtin_Ptr;
                       Metadata : in T) return T with Inline;
   function With_Meta (Builtin  : in Instance;
                       Metadata : in T) return T with Inline;

   function Meta (Item : in Instance) return T with Inline;
   function Builtin (Item : in Instance) return Builtin_Ptr with Inline;

private

   type Instance is new Garbage_Collected.Instance with record
      F_Builtin : Builtin_Ptr;
      F_Meta    : T;
   end record;

   overriding procedure Keep_References (Object : in out Instance) with Inline;

end Types.Builtins;
