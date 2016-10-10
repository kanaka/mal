package body Types is

   function "=" (Left, Right : in Mal_Type) return Boolean is
      (case Left.Kind is
         when Kind_Nil =>
            Right.Kind = Kind_Nil,
         when Kind_Atom =>
            Right.Kind = Kind_Atom
            and then Atoms."=" (Left.Reference, Right.Reference),
         when Kind_Boolean =>
            Right.Kind = Kind_Boolean
            and then Left.Boolean_Value = Right.Boolean_Value,
         when Kind_Number =>
            Right.Kind = Kind_Number
            and then Left.Integer_Value = Right.Integer_Value,
         when Kind_String | Kind_Keyword | Kind_Symbol =>
             Right.Kind = Left.Kind
             and then Strings."=" (Left.S, Right.S),
         when Kind_List | Kind_Vector =>
             Right.Kind in Kind_List | Kind_Vector
             and then Lists."=" (Left.L, Right.L),
         when Kind_Map =>
             Right.Kind = Kind_Map
             and then Maps."=" (Left.Map, Right.Map),
         when Kind_Function =>
             Right.Kind = Kind_Function
             and then Lists."=" (Left.Formals, Right.Formals)
             and then Atoms."=" (Left.Expression, Right.Expression)
             and then Environments."=" (Left.Environment, Right.Environment),
         when Kind_Native =>
             Right.Kind = Kind_Native and then Left.Native = Right.Native,
         when Kind_Macro =>
             Right.Kind = Kind_Macro
             and then Atoms."=" (Left.Mac_Expression, Right.Mac_Expression)
             and then Lists."=" (Left.Mac_Formals, Right.Mac_Formals));

end Types;
