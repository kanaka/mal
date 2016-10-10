with Environments;
with Types; pragma Elaborate_All (Types);

package Core is

   type Eval_Callback_Type is access
     function (Ast : in Types.Mal_Type;
               Env : in Environments.Ptr) return Types.Mal_Type;

   procedure Add_Built_In_Functions
     (Repl          : in Environments.Ptr;
      Eval_Callback : in not null Eval_Callback_Type);

   Exception_Throwed : exception;
   Last_Exception    : Types.Mal_Type;

end Core;
