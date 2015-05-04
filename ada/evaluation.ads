with Envs;
with Types;

package Evaluation is

   function Eval (AParam : Types.Mal_Handle; AnEnv : Envs.Env_Handle)
   return Types.Mal_Handle;

   Evaluation_Error : exception;

   Debug : Boolean := False;

end Evaluation;
