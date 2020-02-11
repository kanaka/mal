limited with Types.Atoms;
limited with Types.Builtins;
limited with Types.Fns;
limited with Types.Maps;
limited with Types.Sequences;
limited with Types.Strings;

package Types is

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

   --  The idea is inspired from the Haskell and OCaml interpreters,
   --  which use a bit to distinguish pointers from integers.  Ada
   --  allows to specify the bit position of each component, but
   --  generating such architecture-dependent definitions seems a lot
   --  of work for MAL.

   --  The Ada tradition is to give explicit names to types, but this
   --  one will be used very often.

   type Kind_Type is
     (Kind_Nil,
      Kind_Atom,
      Kind_Boolean,
      Kind_Number,
      Kind_Symbol,
      Kind_Keyword, Kind_String,
      Kind_List, Kind_Vector,
      Kind_Map,
      Kind_Macro, Kind_Fn, Kind_Builtin_With_Meta, Kind_Builtin);

   subtype Kind_Key      is Kind_Type range Kind_Keyword .. Kind_String;
   subtype Kind_Sequence is Kind_Type range Kind_List .. Kind_Vector;
   subtype Kind_Function is Kind_Type range Kind_Fn .. Kind_Builtin;

   type T;
   type T_Array;
   type Atom_Ptr is not null access Atoms.Instance;
   type Builtin_Ptr is not null access function (Args : in T_Array) return T;
   type Builtin_With_Meta_Ptr is not null access Builtins.Instance;
   type Fn_Ptr is not null access Fns.Instance;
   type Map_Ptr is not null access Maps.Instance;
   type Sequence_Ptr is not null access Sequences.Instance;
   type String_Ptr is not null access Strings.Instance;

   type T (Kind : Kind_Type := Kind_Nil) is record
      case Kind is
      when Kind_Nil =>
         null;
      when Kind_Boolean =>
         Ada_Boolean                 : Boolean;
      when Kind_Number =>
         Number                      : Integer;
      when Kind_Atom =>
         Atom                        : Atom_Ptr;
      when Kind_Key | Kind_Symbol =>
         Str                         : String_Ptr;
      when Kind_Sequence =>
         Sequence                    : Sequence_Ptr;
      when Kind_Map =>
         Map                         : Map_Ptr;
      when Kind_Builtin =>
         Builtin                     : Builtin_Ptr;
      when Kind_Builtin_With_Meta =>
         Builtin_With_Meta           : Builtin_With_Meta_Ptr;
      when Kind_Fn | Kind_Macro =>
         Fn                          : Fn_Ptr;
      end case;
   end record;

   --  Useful for recursive automatic definition of equality for
   --  composite types like the array type below.
   function "=" (Left, Right : in T) return Boolean with Inline;

   Nil : constant T := (Kind => Kind_Nil);

   procedure Keep (Object : in T) with Inline;

   type T_Array is array (Positive range <>) of T;

end Types;
