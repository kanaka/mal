package Types with Pure is

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

end Types;
