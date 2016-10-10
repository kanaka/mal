with Strings; use Strings;

package Names is

   --  Symbols known at compile time are allocated at program
   --  start, in order to avoid repeated allocations and
   --  deallocations during each Read and /Eval/Print cycle. The
   --  reference is kept so each usage does not trigger a search in
   --  the global hash map.

   Ada2           : constant Ptr := Alloc ("ada2");
   Ampersand      : constant Ptr := Alloc ("&");
   Apply          : constant Ptr := Alloc ("apply");
   Argv           : constant Ptr := Alloc ("*ARGV*");
   Assoc          : constant Ptr := Alloc ("assoc");
   Asterisk       : constant Ptr := Alloc ("*");
   Atom           : constant Ptr := Alloc ("atom");
   Catch          : constant Ptr := Alloc ("catch*");
   Concat         : constant Ptr := Alloc ("concat");
   Conj           : constant Ptr := Alloc ("conj");
   Cons           : constant Ptr := Alloc ("cons");
   Contains       : constant Ptr := Alloc ("contains?");
   Count          : constant Ptr := Alloc ("count");
   Def            : constant Ptr := Alloc ("def!");
   Defmacro       : constant Ptr := Alloc ("defmacro!");
   Deref          : constant Ptr := Alloc ("deref");
   Dissoc         : constant Ptr := Alloc ("dissoc");
   Equals         : constant Ptr := Alloc ("=");
   Eval           : constant Ptr := Alloc ("eval");
   First          : constant Ptr := Alloc ("first");
   Fn             : constant Ptr := Alloc ("fn*");
   Get            : constant Ptr := Alloc ("get");
   Greater_Equal  : constant Ptr := Alloc (">=");
   Greater_Than   : constant Ptr := Alloc (">");
   Hash_Map       : constant Ptr := Alloc ("hash-map");
   Host_Language  : constant Ptr := Alloc ("*host-language*");
   Is_Atom        : constant Ptr := Alloc ("atom?");
   Is_Empty       : constant Ptr := Alloc ("empty?");
   Is_False       : constant Ptr := Alloc ("false?");
   Is_Keyword     : constant Ptr := Alloc ("keyword?");
   Is_List        : constant Ptr := Alloc ("list?");
   Is_Map         : constant Ptr := Alloc ("map?");
   Is_Nil         : constant Ptr := Alloc ("nil?");
   Is_Sequential  : constant Ptr := Alloc ("sequential?");
   Is_String      : constant Ptr := Alloc ("string?");
   Is_Symbol      : constant Ptr := Alloc ("symbol?");
   Is_True        : constant Ptr := Alloc ("true?");
   Is_Vector      : constant Ptr := Alloc ("vector?");
   Keys           : constant Ptr := Alloc ("keys");
   Keyword        : constant Ptr := Alloc ("keyword");
   Less_Equal     : constant Ptr := Alloc ("<=");
   Less_Than      : constant Ptr := Alloc ("<");
   Let            : constant Ptr := Alloc ("let*");
   List           : constant Ptr := Alloc ("list");
   Macroexpand    : constant Ptr := Alloc ("macroexpand");
   Mal_Do         : constant Ptr := Alloc ("do");
   Mal_If         : constant Ptr := Alloc ("if");
   Map            : constant Ptr := Alloc ("map");
   Meta           : constant Ptr := Alloc ("meta");
   Minus          : constant Ptr := Alloc ("-");
   Nth            : constant Ptr := Alloc ("nth");
   Plus           : constant Ptr := Alloc ("+");
   Pr_Str         : constant Ptr := Alloc ("pr-str");
   Println        : constant Ptr := Alloc ("println");
   Prn            : constant Ptr := Alloc ("prn");
   Quasiquote     : constant Ptr := Alloc ("quasiquote");
   Quote          : constant Ptr := Alloc ("quote");
   Read_String    : constant Ptr := Alloc ("read-string");
   Readline       : constant Ptr := Alloc ("readline");
   Reset          : constant Ptr := Alloc ("reset!");
   Rest           : constant Ptr := Alloc ("rest");
   Seq            : constant Ptr := Alloc ("seq");
   Slash          : constant Ptr := Alloc ("/");
   Slurp          : constant Ptr := Alloc ("slurp");
   Splice_Unquote : constant Ptr := Alloc ("splice-unquote");
   Str            : constant Ptr := Alloc ("str");
   Swap           : constant Ptr := Alloc ("swap!");
   Symbol         : constant Ptr := Alloc ("symbol");
   Throw          : constant Ptr := Alloc ("throw");
   Time_Ms        : constant Ptr := Alloc ("time-ms");
   Try            : constant Ptr := Alloc ("try*");
   Unquote        : constant Ptr := Alloc ("unquote");
   Vals           : constant Ptr := Alloc ("vals");
   Vector         : constant Ptr := Alloc ("vector");
   With_Meta      : constant Ptr := Alloc ("with-meta");

end Names;
