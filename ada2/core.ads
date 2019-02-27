limited with Environments;
with Types.Mal;

package Core with Elaborate_Body is

   --  Initialization of this package fills Environments.Repl with
   --  built-in functions.

   Eval_Ref : access function (Ast : in Types.Mal.T;
                               Env : in Environments.Ptr)
                              return Types.Mal.T;
   --  Set by the main program at startup.

   Exception_Throwed : exception;
   Last_Exception    : Types.Mal.T;
   --  When the exception is throwed, Last_Exception is set with the
   --  related Data.

end Core;
