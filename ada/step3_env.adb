with Ada.Command_Line;
with Ada.Text_IO;
with Ada.IO_Exceptions;
with Core;
with Envs;
with Eval_Callback;
with Printer;
with Reader;
with Smart_Pointers;
with Types;

procedure Step3_Env is

   use Types;

   function Eval (Param : Types.Mal_Handle; Env : Envs.Env_Handle)
   return Types.Mal_Handle;

   Debug : Boolean := False;


   function Read (Param : String) return Types.Mal_Handle is
   begin
      return Reader.Read_Str (Param);
   end Read;


   function Def_Fn (Args : List_Mal_Type; Env : Envs.Env_Handle)
		   return Mal_Handle is
      Name, Fn_Body, Res : Mal_Handle;
   begin
      Name := Car (Args);
      pragma Assert (Deref (Name).Sym_Type = Sym,
                     "Def_Fn: expected symbol as name");
--      Fn_Body := Car (Deref_List (Cdr (Args)).all);
      Fn_Body := Nth (Args, 1);
      Res := Eval (Fn_Body, Env);
      Envs.Set (Env, Deref_Sym (Name).Get_Sym, Res);
      return Res;
   end Def_Fn;


   function Let_Processing (Args : List_Mal_Type; Env : Envs.Env_Handle)
                          return Mal_Handle is
      Defs, Expr, Res : Mal_Handle;
      E : Envs.Env_Handle;
   begin
      E := Envs.New_Env (Env);
      Defs := Car (Args);
      Deref_List_Class (Defs).Add_Defs (E);
      Expr := Car (Deref_List (Cdr (Args)).all);
      Res := Eval (Expr, E);
      return Res;
   end Let_Processing;


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
                  raise Envs.Not_Found with (" '" &  Sym & "' not found ");
            end;

         when List =>

            return Map (Call_Eval'Unrestricted_Access, Deref_List_Class (Ast).all);

         when others => return Ast;

      end case;

   end Eval_Ast;


   function Eval (Param : Mal_Handle; Env : Envs.Env_Handle)
		 return Mal_Handle is
      First_Elem : Mal_Handle;
   begin

      if Debug then
         Ada.Text_IO.Put_Line ("Evaling " & Deref (Param).To_String);
      end if;

      if Deref (Param).Sym_Type = List and then
	Deref_List (Param).Get_List_Type = List_List then

         declare
            Evaled_H, First_Param, Rest_List : Mal_Handle;
            Param_List : List_Mal_Type;
         begin
            Param_List := Deref_List (Param).all;
            First_Param := Car (Param_List);
            Rest_List := Cdr (Param_List);

            if Deref (First_Param).Sym_Type = Sym and then
               Deref_Sym (First_Param).Get_Sym = "def!" then
               return Def_Fn (Deref_List (Rest_List).all, Env);
            elsif Deref (First_Param).Sym_Type = Sym and then
               Deref_Sym (First_Param).Get_Sym = "let*" then
               return Let_Processing (Deref_List (Rest_List).all, Env);
            else
               -- The APPLY section.
               Evaled_H := Eval_Ast (Param, Env);
               Param_List := Deref_List (Evaled_H).all;
               First_Param := Car (Param_List);
               return Call_Func (Deref_Func (First_Param).all, Cdr (Param_List));
            end if;

         end;

      else -- Not a List_List

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
   S : String (1..Reader.Max_Line_Len);
   Last : Natural;

begin

   if Ada.Command_Line.Argument_Count > 0 then
     if Ada.Command_Line.Argument (1) = "-d" then
        Debug := True;
     end if;
   end if;

   -- Save a function pointer back to the Eval function.
   -- Can't use 'Access here because of Ada rules but 'Unrestricted_Access is OK
   -- as we know Eval will be in scope for the lifetime of the program.
   Eval_Callback.Eval := Eval'Unrestricted_Access;

   Repl_Env := Envs.New_Env;

   Core.Init (Repl_Env);

   loop
      Ada.Text_IO.Put ("user> ");
      Ada.Text_IO.Get_Line (S, Last);
      Ada.Text_IO.Put_Line (Rep (S (1..Last), Repl_Env));
   end loop;

exception
   when Ada.IO_Exceptions.End_Error => null;
   -- i.e. exit without textual output
end Step3_Env;
