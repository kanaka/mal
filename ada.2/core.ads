with Types.Symbols;
with Types.Mal;

package Core with Elaborate_Body is

   type Binding is record
      Symbol  : Types.Symbols.Ptr;
      Builtin : Types.Mal.Builtin_Ptr;
   end record;

   type Binding_List is array (Positive range <>) of Binding;

   function Ns return Binding_List;
   --  A list of built-in symbols and functionse.
   --  A constant would make sense, but
   --  * implementing it in the private part

   Exception_Throwed : exception;
   Last_Exception    : Types.Mal.T := Types.Mal.Nil;
   --  When the "throw" builtin is executed, it assigns its argument
   --  to Last_Exception, then raises this Ada exception.

end Core;
