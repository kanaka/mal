with Envs;
with Types;

package Evaluation is

   function Eval (Param : Types.Mal_Handle; Env : Envs.Env_Handle)
   return Types.Mal_Handle;

   Evaluation_Error : exception;

   Debug : Boolean := False;

end Evaluation;
