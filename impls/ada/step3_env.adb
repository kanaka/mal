with Ada.Command_Line;
with Ada.Exceptions;
with Ada.Text_IO;
with Envs;
with Eval_Callback;
with Printer;
with Reader;
with Smart_Pointers;
with Types;

procedure Step3_Env is

   use Types;

   -- primitive functions on Smart_Pointer,
   function "+" is new Arith_Op ("+", "+");
   function "-" is new Arith_Op ("-", "-");
   function "*" is new Arith_Op ("*", "*");
   function "/" is new Arith_Op ("/", "/");

   -- Take a list with two parameters and produce a single result
   -- using the Op access-to-function parameter.
   function Reduce2
     (Op : Binary_Func_Access; LH : Mal_Handle)
   return Mal_Handle is
      Left, Right : Mal_Handle;
      L, Rest_List : List_Mal_Type;
   begin
      L := Deref_List (LH).all;
      Left := Car (L);
      Rest_List := Deref_List (Cdr (L)).all;
      Right := Car (Rest_List);
      return Op (Left, Right);
   end Reduce2;


   function Plus (Rest_Handle : Mal_Handle)
   return Types.Mal_Handle is
   begin
       return Reduce2 (Step3_Env."+"'Unrestricted_Access, Rest_Handle);
   end Plus;


   function Minus (Rest_Handle : Mal_Handle)
   return Types.Mal_Handle is
   begin
       return Reduce2 (Step3_Env."-"'Unrestricted_Access, Rest_Handle);
   end Minus;


   function Mult (Rest_Handle : Mal_Handle)
   return Types.Mal_Handle is
   begin
       return Reduce2 (Step3_Env."*"'Unrestricted_Access, Rest_Handle);
   end Mult;


   function Divide (Rest_Handle : Mal_Handle)
   return Types.Mal_Handle is
   begin
       return Reduce2 (Step3_Env."/"'Unrestricted_Access, Rest_Handle);
   end Divide;


   function Eval (Param : Types.Mal_Handle; Env : Envs.Env_Handle)
   return Types.Mal_Handle;


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
      pragma Assert (Deref (Ast).Sym_Type = List); -- list, map or vector
      return Map (Call_Eval'Unrestricted_Access, Deref_List_Class (Ast).all);
   end Eval_Ast;


   function Eval (Param : Mal_Handle; Env : Envs.Env_Handle)
		 return Mal_Handle is
      Ast : Mal_Handle renames Param; --  Historic
   begin
      declare
         M : Mal_Handle;
         B : Boolean;
      begin
         M := Envs.Get (Env, "DEBUG-EVAL");
         case Deref (M).Sym_Type is
            when Bool   => B := Deref_Bool (M).Get_Bool;
            when Nil    => B := False;
            when others => B := True;
         end case;
         if B then
            Ada.Text_IO.Put_Line ("EVAL: " & Deref (Param).To_String);
         end if;
      exception
         when Envs.Not_Found => null;
      end;

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
         case Deref_List (Param).Get_List_Type is
         when Hashed_List | Vector_List =>
            return Eval_Ast (Param, Env);
         when List_List =>

         declare
            Evaled_H, First_Param, Rest_List : Mal_Handle;
            Param_List : List_Mal_Type;
         begin
            Param_List := Deref_List (Param).all;

            -- Deal with empty list..
            if Param_List.Length = 0 then
               return Param;
            end if;

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

         end case;
      when others => -- not a list, map, symbol or vector
         return Param;
      end case;
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


   procedure Init (Env : Envs.Env_Handle) is
   begin

      Envs.Set (Env,
           "+",
           New_Func_Mal_Type ("+", Plus'Unrestricted_Access));

      Envs.Set (Env,
           "-",
           New_Func_Mal_Type ("-", Minus'Unrestricted_Access));

      Envs.Set (Env,
           "*",
           New_Func_Mal_Type ("*", Mult'Unrestricted_Access));

      Envs.Set (Env,
           "/",
           New_Func_Mal_Type ("/", Divide'Unrestricted_Access));

   end Init;


   Repl_Env : Envs.Env_Handle;
begin

   -- Save a function pointer back to the Eval function.
   -- Can't use 'Access here because of Ada rules but 'Unrestricted_Access is OK
   -- as we know Eval will be in scope for the lifetime of the program.
   Eval_Callback.Eval := Eval'Unrestricted_Access;

   Repl_Env := Envs.New_Env;

   Init (Repl_Env);

   loop
      begin
         Ada.Text_IO.Put ("user> ");
         exit when Ada.Text_IO.End_Of_File;
         Ada.Text_IO.Put_Line (Rep (Ada.Text_IO.Get_Line, Repl_Env));
      exception
         when E : others =>
            Ada.Text_IO.Put_Line
              (Ada.Text_IO.Standard_Error,
               Ada.Exceptions.Exception_Information (E));
      end;
   end loop;
end Step3_Env;
