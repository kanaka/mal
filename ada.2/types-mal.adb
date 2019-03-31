with Types.Atoms;
with Types.Builtins;
with Types.Maps;
with Types.Sequences;
with Types.Fns;

package body Types.Mal is

   use type Ada.Strings.Unbounded.Unbounded_String;
   use type Maps.Instance;
   use type Sequences.Instance;
   use type Symbols.Ptr;

   ----------------------------------------------------------------------

   function "=" (Left, Right : in T) return Boolean
   is (case Left.Kind is
      when Kind_Nil =>
         Right.Kind = Kind_Nil,
      when Kind_Boolean =>
         Right.Kind = Kind_Boolean
         and then Left.Ada_Boolean = Right.Ada_Boolean,
      when Kind_Number =>
         Right.Kind = Kind_Number and then Left.Number = Right.Number,
      when Kind_Symbol =>
         Right.Kind = Kind_Symbol and then Left.Symbol = Right.Symbol,
      when Kind_Key =>
         Right.Kind = Left.Kind and then Left.S = Right.S,
      --  Here comes the part that differs from the predefined equality.
      when Kind_Sequence =>
         Right.Kind in Kind_Sequence
         and then Left.Sequence.all = Right.Sequence.all,
      when Kind_Map =>
         Right.Kind = Kind_Map and then Left.Map.all = Right.Map.all,
      --  Also, comparing functions is an interesting problem.
      when others =>
         False);

   procedure Keep (Object : in T) is
   begin
      case Object.Kind is
      when Kind_Nil | Kind_Boolean | Kind_Number | Kind_Key | Kind_Builtin
        | Kind_Symbol =>
         null;
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

end Types.Mal;
