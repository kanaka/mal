with Ada.Text_IO;
with Ada.IO_Exceptions;
with Ada.Exceptions;
with Core;
with Envs;
with Eval_Callback;
with Printer;
with Reader;
with Smart_Pointers;
with Types;

procedure Step2_Eval is

   use Types;

   function Eval (Param : Types.Mal_Handle; Env : Envs.Env_Handle)
   return Types.Mal_Handle;

   Debug : Boolean := False;


   function Read (Param : String) return Types.Mal_Handle is
   begin
      return Reader.Read_Str (Param);
   end Read;


   function Eval_As_Boolean (MH : Mal_Handle) return Boolean is
      Res : Boolean;
   begin
      case Deref (MH).Sym_Type is
         when Bool => 
            Res := Deref_Bool (MH).Get_Bool;
         when Sym =>
            return not (Deref_Sym (MH).Get_Sym = "nil");
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
            Evaled_H, First_Param : Mal_Handle;
            Evaled_List : List_Mal_Type;
         begin
            Evaled_H := Eval_Ast (Param, Env);
            Evaled_List := Deref_List (Evaled_H).all;
            First_Param := Car (Evaled_List);
            return Call_Func (Deref_Func (First_Param).all, Cdr (Evaled_List), Env);
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

   -- Save a function pointer back to the Eval function.
   -- Can't use 'Access here because of Ada rules but 'Unrestricted_Access is OK
   -- as we know Eval will be in scope for the lifetime of the program.
   Eval_Callback.Eval := Eval'Unrestricted_Access;

   Repl_Env := Envs.New_Env;

   Core.Init (Repl_Env);

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

exception
   when Ada.IO_Exceptions.End_Error => null;
   -- i.e. exit without textual output
end Step2_Eval;
