with Ada.Command_Line;
with Ada.Exceptions;
with Ada.Text_IO;
with Ada.IO_Exceptions;
with Core;
with Envs;
with Eval_Callback;
with Printer;
with Reader;
with Smart_Pointers;
with Types;


procedure Step6_File is


   use Types;


   function Read (Param : String) return Types.Mal_Handle is
   begin
      return Reader.Read_Str (Param);
   end Read;


   -- Forward declaration of Eval.
   function Eval (AParam : Types.Mal_Handle; AnEnv : Envs.Env_Handle)
   return Types.Mal_Handle;


   Debug : Boolean := False;


   function Def_Fn (Args : List_Mal_Type; Env : Envs.Env_Handle)
		   return Mal_Handle is
      Name, Fn_Body, Res : Mal_Handle;
   begin
      Name := Car (Args);
      pragma Assert (Deref (Name).Sym_Type = Sym,
                     "Def_Fn: expected atom as name");
      Fn_Body := Nth (Args, 1);
      Res := Eval (Fn_Body, Env);
      Envs.Set (Env, Deref_Sym (Name).Get_Sym, Res);
      return Res;
   end Def_Fn;


   function Eval_As_Boolean (MH : Mal_Handle) return Boolean is
      Res : Boolean;
   begin
      case Deref (MH).Sym_Type is
         when Bool => 
            Res := Deref_Bool (MH).Get_Bool;
         when Nil =>
            return False;
--         when List =>
--            declare
--               L : List_Mal_Type;
--            begin
--               L := Deref_List (MH).all;
--               Res := not Is_Null (L);
--            end;
         when others => -- Everything else
            Res := True;
      end case;
      return Res;
   end Eval_As_Boolean;


   function Eval_Ast
     (Ast : Mal_Handle; Env : Envs.Env_Handle)
     return Mal_Handle is

      function Call_Eval (A : Mal_Handle) return Mal_Handle is
      begin
         return Eval (A, Env);
      end Call_Eval;

   begin

      case Deref (Ast).Sym_Type is

         when Sym =>

            declare
               Sym : Mal_String := Deref_Sym (Ast).Get_Sym;
            begin
               -- if keyword, return it. Otherwise look it up in the environment.
               if Sym(1) = ':' then
                  return Ast;
               else
                  return Envs.Get (Env, Sym);
               end if;
            exception
               when Envs.Not_Found =>
                  raise Envs.Not_Found with ("'" &  Sym & "' not found");
            end;

         when List =>

            return Map (Call_Eval'Unrestricted_Access, Deref_List_Class (Ast).all);

         when others => return Ast;

      end case;

   end Eval_Ast;


   function Eval (AParam : Mal_Handle; AnEnv : Envs.Env_Handle)
		 return Mal_Handle is
      Param : Mal_Handle;
      Env : Envs.Env_Handle;
      First_Param, Rest_Params : Mal_Handle;
      Rest_List, Param_List : List_Mal_Type;
   begin

      Param := AParam;
      Env := AnEnv;

      <<Tail_Call_Opt>>

      if Debug then
         Ada.Text_IO.Put_Line ("Evaling " & Deref (Param).To_String);
      end if;

      if Deref (Param).Sym_Type = List and then
	Deref_List (Param).Get_List_Type = List_List then

         Param_List := Deref_List (Param).all;

         -- Deal with empty list..
         if Param_List.Length = 0 then
	    return Param;
         end if;

         First_Param := Car (Param_List);
         Rest_Params := Cdr (Param_List);
         Rest_List := Deref_List (Rest_Params).all;

         if Deref (First_Param).Sym_Type = Sym and then
            Deref_Sym (First_Param).Get_Sym = "def!" then
 	    return Def_Fn (Rest_List, Env);
         elsif Deref (First_Param).Sym_Type = Sym and then
               Deref_Sym (First_Param).Get_Sym = "let*" then
            declare
               Defs, Expr, Res : Mal_Handle;
               E : Envs.Env_Handle;
            begin
               E := Envs.New_Env (Env);
               Defs := Car (Rest_List);
               Deref_List_Class (Defs).Add_Defs (E);
               Expr := Car (Deref_List (Cdr (Rest_List)).all);
	       Param := Expr;
	       Env := E;
	       goto Tail_Call_Opt;
               -- was:
               -- Res := Eval (Expr, E);
               -- return Res;
            end;
         elsif Deref (First_Param).Sym_Type = Sym and then
               Deref_Sym (First_Param).Get_Sym = "do" then
            declare
               D : List_Mal_Type;
               E : Mal_Handle;
            begin

               if Debug then
                  Ada.Text_IO.Put_Line ("Do-ing " & To_String (Rest_List));
               end if;

               if Is_Null (Rest_List) then
                  return Rest_Params;
               end if;

               -- Loop processes Evals all but last entry
               D := Rest_List;
               loop
                  E := Car (D);
                  D := Deref_List (Cdr (D)).all;
               exit when Is_Null (D);
                  E := Eval (E, Env);
               end loop;

               Param := E;
               goto Tail_Call_Opt;

            end;
         elsif Deref (First_Param).Sym_Type = Sym and then
               Deref_Sym (First_Param).Get_Sym = "if" then
 	    declare
	       Args : List_Mal_Type := Rest_List;

	       Cond, True_Part, False_Part : Mal_Handle;
	       Cond_Bool : Boolean;
	       pragma Assert (Length (Args) = 2 or Length (Args) = 3,
			      "If_Processing: not 2 or 3 parameters");
	       L : List_Mal_Type;
	    begin

	       Cond := Eval (Car (Args), Env);

	       Cond_Bool := Eval_As_Boolean (Cond);

	       if Cond_Bool then
	          L := Deref_List (Cdr (Args)).all;

	          Param := Car (L);
		  goto Tail_Call_Opt;
		  -- was: return Eval (Car (L), Env);
	       else
		  if Length (Args) = 3 then
		     L := Deref_List (Cdr (Args)).all;
		     L := Deref_List (Cdr (L)).all;

		     Param := Car (L);
		     goto Tail_Call_Opt;
		     -- was: return Eval (Car (L), Env);
	          else
		     return New_Nil_Mal_Type;
	          end if;
	       end if;
	    end;

         elsif Deref (First_Param).Sym_Type = Sym and then
               Deref_Sym (First_Param).Get_Sym = "fn*" then

            return New_Lambda_Mal_Type
                     (Params => Car (Rest_List),
                      Expr => Nth (Rest_List, 1),
                      Env => Env);

         else

            -- The APPLY section.
            declare
               Evaled_H : Mal_Handle;
            begin
               Evaled_H := Eval_Ast (Param, Env);

               Param_List := Deref_List (Evaled_H).all;

               First_Param := Car (Param_List);
               Rest_Params := Cdr (Param_List);
               Rest_List := Deref_List (Rest_Params).all;

               if Deref (First_Param).Sym_Type = Func then
                  return Call_Func (Deref_Func (First_Param).all, Rest_Params);
               elsif Deref (First_Param).Sym_Type = Lambda then
                  declare

                     L : Lambda_Mal_Type;
                     E : Envs.Env_Handle;
                     Param_Names : List_Mal_Type;
                     Res : Mal_Handle;

                  begin

                     L := Deref_Lambda (First_Param).all;
                     E := Envs.New_Env (L.Get_Env);

                     Param_Names := Deref_List (L.Get_Params).all;

                     if Envs.Bind (E, Param_Names, Deref_List (Rest_Params).all) then

			Param := L.Get_Expr;
                        Env := E;
			goto Tail_Call_Opt;
                        -- was: return Eval (L.Get_Expr, E); 

                     else

                        raise Mal_Exception with "Bind failed in Apply";

                     end if;

                  end;

               else  -- neither a Lambda or a Func
                  raise Mal_Exception;
               end if;

            end;

         end if;

      else

         return Eval_Ast (Param, Env);

      end if;

   end Eval;


   function Print (Param : Types.Mal_Handle) return String is
   begin
      return Printer.Pr_Str (Param);
   end Print;


   function Rep (Param : String; Env : Envs.Env_Handle) return String is
      AST, Evaluated_AST : Types.Mal_Handle;
   begin

      AST := Read (Param);

      if Types.Is_Null (AST) then
         return "";
      else
         Evaluated_AST := Eval (AST, Env);
         return Print (Evaluated_AST);
      end if;

   end Rep; 


   Repl_Env : Envs.Env_Handle;


   -- These two ops use Repl_Env directly.


   procedure RE (Str : Mal_String) is
      Discarded : Mal_Handle;
   begin
      Discarded := Eval (Read (Str), Repl_Env);
   end RE;


   function Do_Eval (Rest_Handle : Mal_Handle)
   return Types.Mal_Handle is
      First_Param : Mal_Handle;
      Rest_List : Types.List_Mal_Type;
   begin
      Rest_List := Deref_List (Rest_Handle).all;
      First_Param := Car (Rest_List);
      return Eval_Callback.Eval.all (First_Param, Repl_Env);
   end Do_Eval;


   S : String (1..Reader.Max_Line_Len);
   Last : Natural;
   Cmd_Args, File_Param : Natural;
   Command_Args : Types.Mal_Handle;
   Command_List : Types.List_Ptr;
   File_Processed : Boolean := False;

begin

   -- Save a function pointer back to the Eval function.
   -- Can't use 'Access here because of Ada rules but 'Unrestricted_Access is OK
   -- as we know Eval will be in scope for the lifetime of the program.
   Eval_Callback.Eval := Eval'Unrestricted_Access;

   Repl_Env := Envs.New_Env;

   Core.Init (Repl_Env);

   -- Register the eval command.  This needs to be done here rather than Core.Init
   -- as it requires direct access to Repl_Env.
   Envs.Set (Repl_Env, "eval", New_Func_Mal_Type ("eval", Do_Eval'Unrestricted_Access));

   RE ("(def! not (fn* (a) (if a false true)))");

   RE ("(def! load-file (fn* (f) (eval (read-string (str ""(do "" (slurp f) "")"")))))");

   -- Command line processing.

   Cmd_Args := 0;
   Command_Args := Types.New_List_Mal_Type (Types.List_List);
   Command_List := Types.Deref_List (Command_Args);

   while Ada.Command_Line.Argument_Count > Cmd_Args loop

     Cmd_Args := Cmd_Args + 1;
     if Ada.Command_Line.Argument (Cmd_Args) = "-d" then
        Debug := True;
     elsif Ada.Command_Line.Argument (Cmd_Args) = "-e" then
        Envs.Debug := True;
     elsif not File_Processed then
        File_Param := Cmd_Args;
        File_Processed := True;
     else
        Command_List.Append
          (Types.New_String_Mal_Type (Ada.Command_Line.Argument (Cmd_Args)));
     end if;

   end loop;

   Envs.Set (Repl_Env, "*ARGV*", Command_Args);

   if File_Processed then
      RE ("(load-file """ & Ada.Command_Line.Argument (File_Param) & """)");
   else
      loop
         begin
            Ada.Text_IO.Put ("user> ");
            Ada.Text_IO.Get_Line (S, Last);
            Ada.Text_IO.Put_Line (Rep (S (1..Last), Repl_Env));
         exception
            when Ada.IO_Exceptions.End_Error => raise;
            when E : others =>
               Ada.Text_IO.Put_Line
                 (Ada.Text_IO.Standard_Error,
                  Ada.Exceptions.Exception_Information (E));
         end;
      end loop;
   end if;

exception
   when Ada.IO_Exceptions.End_Error => null;
   -- i.e. exit without textual output
end Step6_File;
