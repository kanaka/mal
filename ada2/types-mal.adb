package body Types.Mal is

   use type Ada.Strings.Unbounded.Unbounded_String;
   use type Lists.Ptr;
   use type Maps.Ptr;
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
         Right.Kind = Kind_Number and then Left.Ada_Number = Right.Ada_Number,
      when Kind_Symbol =>
         Right.Kind = Kind_Symbol and then Left.Symbol = Right.Symbol,
      --  Here is the part that differs from the predefined equality.
      when Kind_Keyword | Kind_String =>
         Right.Kind = Left.Kind and then Left.S = Right.S,
      when Kind_List | Kind_Vector =>
         Right.Kind in Kind_List | Kind_Vector and then Left.L = Right.L,
      when Kind_Map =>
         Right.Kind = Kind_Map and then Left.Map = Right.Map,
      when others =>
         False);

end Types.Mal;
