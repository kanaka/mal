pragma Warnings (Off, "no entities of ""Types.*"" are referenced");
with Types.Atoms;
with Types.Builtins;
with Types.Fns;
with Types.Maps;
with Types.Sequences;
pragma Warnings (On, "no entities of ""Types.*"" are referenced");
with Types.Strings;

package body Types is

   function "=" (Left, Right : in T) return Boolean
   is (case Left.Kind is
      when Kind_Nil =>
         Right.Kind = Kind_Nil,
      when Kind_Boolean =>
         Right.Kind = Kind_Boolean
         and then Left.Ada_Boolean = Right.Ada_Boolean,
      when Kind_Number =>
         Right.Kind = Kind_Number and then Left.Number = Right.Number,
      --  Here comes the part that differs from the predefined equality.
      when Kind_Key | Kind_Symbol =>
         Right.Kind = Left.Kind
         and then Strings.Same_Contents (Left.Str, Right.Str),
      when Kind_Sequence =>
         Right.Kind in Kind_Sequence
         and then (Left.Sequence = Right.Sequence
            or else Sequences."=" (Left.Sequence.all, Right.Sequence.all)),
      when Kind_Map =>
         Right.Kind = Kind_Map
         and then (Left.Map = Right.Map
            or else Maps."=" (Left.Map.all, Right.Map.all)),
      --  Also, comparing functions is an interesting problem.
      when others =>
         False);

   procedure Keep (Object : in T) is
      --  No dynamic dispatching happens here.
   begin
      case Object.Kind is
      when Kind_Nil | Kind_Boolean | Kind_Number | Kind_Builtin =>
         null;
      when Kind_Key | Kind_Symbol =>
         Object.Str.all.Keep;
      when Kind_Atom =>
         Object.Atom.all.Keep;
      when Kind_Sequence =>
         Object.Sequence.all.Keep;
      when Kind_Map =>
         Object.Map.all.Keep;
      when Kind_Builtin_With_Meta =>
         Object.Builtin_With_Meta.all.Keep;
      when Kind_Fn | Kind_Macro =>
         Object.Fn.all.Keep;
      end case;
   end Keep;

end Types;
