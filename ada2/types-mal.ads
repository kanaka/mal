with Ada.Strings.Unbounded;

with Types.Atoms;
with Types.Builtins;
with Types.Functions;
with Types.Lists;
with Types.Maps;
with Types.Symbols;

package Types.Mal is

   --  A type with a default value for the discriminant is the Ada
   --  equivalent of a C union. It uses a fixed size, and allows
   --  efficient arrays. A class hierarchy would make this impossible,
   --  for little gain.
   --  Native types may seem to consume too much memory, but
   --  1/ they require no allocation/deallocation.
   --  2/ the overhead would actually be higher with an intermediate
   --     reference (the size of the pointer plus the size of the native
   --     type, while an union uses the minimum of both and a single
   --     memory area ).
   --  Each instance has the size required for the largest possible
   --  value, so subtypes should attempt to reduce their size when
   --  possible (see Types.Symbols for such a compromise).

   --  The idea is inspired from the Haskell and OCaml interpreters,
   --  which use a bit to distinguish pointers from integers.  Ada
   --  allows to specify the bit position of each component, but
   --  generating such architecture-dependent definitions seems a lot
   --  of work for MAL.

   --  The Ada tradition is to give explicit names to types, but this
   --  one will be used very often, and almost each package declares
   --  an "use Types;" clause, so Mal.T will do.

   --  The only problem with a hidden discriminant is that "in out"
   --  parameters cannot be reaffected with a different discriminant.
   --  Eval would be more efficient with "in out" parameters than with
   --  "in" parameters and a result, because lots of reference
   --  counting would be spared, and the implementation would be able
   --  to reuse dynamic memory more efficiently.  Environments, and
   --  some list/map operations already attempt such reuse behind the
   --  curtain.

   --  This would obfuscate the implementation of a functional
   --  language, and require deep changes (the discriminant can be
   --  changed for an in out or access parameter).

   type T (Kind : Kind_Type := Kind_Nil) is record
      case Kind is
         when Kind_Nil =>
            null;
         when Kind_Boolean =>
            Ada_Boolean               : Boolean;
         when Kind_Number =>
            Ada_Number                : Integer;
         when Kind_Atom =>
            Atom                      : Atoms.Ptr;
         when Kind_Keyword | Kind_String =>
            S                         : Ada.Strings.Unbounded.Unbounded_String;
         when Kind_Symbol =>
            Symbol                    : Symbols.Ptr;
         when Kind_List | Kind_Vector =>
            L                         : Lists.Ptr;
         when Kind_Map =>
            Map                       : Maps.Ptr;
         when Kind_Builtin =>
            Builtin                   : Builtins.Ptr;
         when Kind_Builtin_With_Meta =>
            Builtin_With_Meta         : Builtins.Ptr_With_Meta;
         when Kind_Function | Kind_Macro =>
            Function_Value            : Functions.Ptr;
      end case;
   end record;

   --  Useful for recursive automatic definition of equality for
   --  composite types like the array type below.
   function "=" (Left, Right : in T) return Boolean with Inline;

   Nil : constant T := (Kind => Kind_Nil);

   type T_Array is array (Positive range <>) of T;

end Types.Mal;
