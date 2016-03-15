with Envs;
with Types;

package Evaluation is

   function Eval (AParam : Types.Mal_Handle; AnEnv : Envs.Env_Handle)
   return Types.Mal_Handle;

   Debug : Boolean := False;

end Evaluation;
