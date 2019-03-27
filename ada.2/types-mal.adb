package body Types.Mal is

   use type Ada.Strings.Unbounded.Unbounded_String;
   use type Maps.Ptr;
   use type Sequences.Ptr;
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
         Right.Kind in Kind_Sequence and then Left.Sequence = Right.Sequence,
      when Kind_Map =>
         Right.Kind = Kind_Map and then Left.Map = Right.Map,
      --  Also, comparing functions is an interesting problem.
      when others =>
         False);

end Types.Mal;
