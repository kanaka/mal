with Garbage_Collected;
with Types.Mal;

package Types.Builtins is

   --  Types.Mal.Builtin_Ptr is efficient and sufficient for most
   --  purposes, as counting references is a waste of time for native
   --  functions. The controlled type below is only useful when one
   --  has the silly idea to add metadata to a built-in.

   type Instance is new Garbage_Collected.Instance with private;

   function With_Meta (Builtin  : in Mal.Builtin_Ptr;
                       Metadata : in Mal.T) return Mal.T with Inline;
   function With_Meta (Item     : in Instance;
                       Metadata : in Mal.T) return Mal.T with Inline;
   function Meta (Item : in Instance) return Mal.T with Inline;
   function Builtin (Item : in Instance) return Mal.Builtin_Ptr with Inline;

private

   type Instance is new Garbage_Collected.Instance with record
      F_Builtin : Mal.Builtin_Ptr;
      F_Meta    : Mal.T;
   end record;
   overriding procedure Keep_References (Object : in out Instance) with Inline;

end Types.Builtins;
