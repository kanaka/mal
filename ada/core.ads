with Envs;

package Core is

   -- Init puts core functions into a new Env.
   procedure Init (Repl_Env : Envs.Env_Handle);

   Evaluation_Error : exception;

end Core;
