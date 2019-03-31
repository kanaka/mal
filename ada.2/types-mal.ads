with Ada.Strings.Unbounded;

limited with Types.Atoms;
limited with Types.Builtins;
limited with Types.Fns;
limited with Types.Maps;
limited with Types.Sequences;
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

   type T;
   type T_Array;

   type Atom_Ptr is access Atoms.Instance;
   type Builtin_Ptr is access function (Args : in T_Array) return T;
   type Builtin_With_Meta_Ptr is access Builtins.Instance;
   type Fn_Ptr is access Fns.Instance;
   type Map_Ptr is access Maps.Instance;
   type Sequence_Ptr is access Sequences.Instance;

   type T (Kind : Kind_Type := Kind_Nil) is record
      case Kind is
         when Kind_Nil =>
            null;
         when Kind_Boolean =>
            Ada_Boolean               : Boolean;
         when Kind_Number =>
            Number                    : Integer;
         when Kind_Atom =>
            Atom                      : Atom_Ptr;
         when Kind_Key =>
            S                         : Ada.Strings.Unbounded.Unbounded_String;
         when Kind_Symbol =>
            Symbol                    : Symbols.Ptr;
         when Kind_Sequence =>
            Sequence                  : Sequence_Ptr;
         when Kind_Map =>
            Map                       : Map_Ptr;
         when Kind_Builtin =>
            Builtin                   : Builtin_Ptr;
         when Kind_Builtin_With_Meta =>
            Builtin_With_Meta         : Builtin_With_Meta_Ptr;
         when Kind_Fn | Kind_Macro =>
            Fn                        : Fn_Ptr;
      end case;
   end record;

   --  Useful for recursive automatic definition of equality for
   --  composite types like the array type below.
   function "=" (Left, Right : in T) return Boolean with Inline;

   Nil : constant T := (Kind => Kind_Nil);

   procedure Keep (Object : in Mal.T) with Inline;

   type T_Array is array (Positive range <>) of T;

end Types.Mal;
