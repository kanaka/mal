package Types.Symbols.Names is

   --  These symbols are used once by Read/Eval/Print cycle.  Declare
   --  them here in order to avoid an allocation and a desallocation
   --  during each call of eval.
   --  The built-in functions declared in Core will remain allocated
   --  during the lifetime of the program and do not require this.

   --  A separate package is required because the constructor must be
   --  callable, and a child package makes sense because without this
   --  problem, these definition would be in Symbols.
   Ampersand      : constant Ptr := Constructor ("&");
   Catch          : constant Ptr := Constructor ("catch*");
   Def            : constant Ptr := Constructor ("def!");
   Defmacro       : constant Ptr := Constructor ("defmacro!");
   Fn             : constant Ptr := Constructor ("fn*");
   Let            : constant Ptr := Constructor ("let*");
   Macroexpand    : constant Ptr := Constructor ("macroexpand");
   Mal_Do         : constant Ptr := Constructor ("do");
   Mal_If         : constant Ptr := Constructor ("if");
   Quasiquote     : constant Ptr := Constructor ("quasiquote");
   Quote          : constant Ptr := Constructor ("quote");
   Splice_Unquote : constant Ptr := Constructor ("splice-unquote");
   Try            : constant Ptr := Constructor ("try*");
   Unquote        : constant Ptr := Constructor ("unquote");

   --  These are used by both Core and Reader. Spare a search.
   Deref          : constant Ptr := Constructor ("deref");
   With_Meta      : constant Ptr := Constructor ("with-meta");

end Types.Symbols.Names;
