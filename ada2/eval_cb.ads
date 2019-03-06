with Envs;
with Types.Mal;

package Eval_Cb is

   Cb : access function (Ast : in Types.Mal.T;
                         Env : in Envs.Ptr) return Types.Mal.T;
   --  The main program must register this global callback to the main
   --  eval function before some built-in functions are executed.

end Eval_Cb;
