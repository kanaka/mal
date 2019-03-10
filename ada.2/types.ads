package Types with Pure is

   --  Similar kinds should be consecutive for efficient case
   --  statements.
   type Kind_Type is
     (Kind_Nil,
      Kind_Atom,
      Kind_Boolean,
      Kind_Number,
      Kind_String, Kind_Symbol, Kind_Keyword,
      Kind_List, Kind_Vector,
      Kind_Map,
      Kind_Macro, Kind_Function, Kind_Builtin_With_Meta, Kind_Builtin);

   --  Raised when a program attempts to execute something else than a
   --  function or a macro, or when a builtin receives a bad argument
   --  count, type or value.
   Argument_Error : exception;

end Types;
