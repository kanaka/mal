with Ada.Exceptions;
with Ada.Text_IO.Unbounded_IO;

with Core;
with Envs;
with Eval_Cb;
with Printer;
with Reader;
with Readline;
with Types.Functions;
with Types.Lists;
with Types.Mal;
with Types.Maps;
with Types.Symbols.Names;

procedure Step4_If_Fn_Do is

   use Types;

   function Read return Mal.T with Inline;

   function Eval (Ast : in Mal.T;
                  Env : in Envs.Ptr) return Mal.T;

   procedure Print (Ast : in Mal.T) with Inline;

   procedure Rep (Env : in Envs.Ptr) with Inline;

   function Eval_List_Elts is new Lists.Generic_Eval (Envs.Ptr, Eval);
   function Eval_Map_Elts  is new Maps.Generic_Eval  (Envs.Ptr, Eval);

   --  Procedural form of Eval.
   --  Convenient when the result of eval is of no interest.
   procedure Eval_P (Ast : in Mal.T;
                     Env : in Envs.Ptr) with Inline;

   ----------------------------------------------------------------------

   function Eval (Ast : in Mal.T;
                  Env : in Envs.Ptr) return Mal.T
   is
      use type Symbols.Ptr;
      First          : Mal.T;
   begin
      --  Ada.Text_IO.New_Line;
      --  Ada.Text_IO.Put ("EVAL: ");
      --  Print (Ast);
      --  Envs.Dump_Stack;
      case Ast.Kind is
      when Kind_Nil | Kind_Atom | Kind_Boolean | Kind_Number | Kind_String
        | Kind_Keyword | Kind_Macro | Kind_Function
        | Kind_Builtin_With_Meta | Kind_Builtin =>
         return Ast;
      when Kind_Symbol =>
         return Env.Get (Ast.Symbol);
      when Kind_Map =>
         return Eval_Map_Elts (Ast.Map, Env);
      when Kind_Vector =>
         return (Kind_Vector, Eval_List_Elts (Ast.List, Env));
      when Kind_List =>
         null;
      end case;

      --  Ast is a list.
      if Ast.List.Length = 0 then
         return Ast;
      end if;
      First := Ast.List.Element (1);

      --  Special forms
      --  Ast is a non-empty list, First is its first element.
      case First.Kind is
      when Kind_Symbol =>
         if First.Symbol = Symbols.Names.Def then
            if Ast.List.Length /= 3 then
               raise Argument_Error with "def!: expects 2 arguments";
            elsif Ast.List.Element (2).Kind /= Kind_Symbol then
               raise Argument_Error with "def!: arg 1 must be a symbol";
            end if;
            return R : constant Mal.T := Eval (Ast.List.Element (3), Env) do
               Env.Set (Ast.List.Element (2).Symbol, R);
            end return;
         elsif First.Symbol = Symbols.Names.Mal_Do then
            if Ast.List.Length = 1 then
               raise Argument_Error with "do: expects at least 1 argument";
            end if;
            for I in 2 .. Ast.List.Length - 1 loop
               Eval_P (Ast.List.Element (I), Env);
            end loop;
            return Eval (Ast.List.Element (Ast.List.Length), Env);
         elsif First.Symbol = Symbols.Names.Fn then
            if Ast.List.Length /= 3 then
               raise Argument_Error with "fn*: expects 3 arguments";
            elsif Ast.List.Element (2).Kind not in Kind_List | Kind_Vector then
               raise Argument_Error with "fn*: arg 1 must be a list or vector";
            elsif (for some F in 1 .. Ast.List.Element (2).List.Length =>
                     Ast.List.Element (2).List.Element (F).Kind /= Kind_Symbol)
            then
               raise Argument_Error with "fn*: arg 2 must contain symbols";
            end if;
            return Functions.New_Function (Params => Ast.List.Element (2).List,
                                           Ast    => Ast.List.Element (3),
                                           Env    => Env.New_Closure);
         elsif First.Symbol = Symbols.Names.Mal_If then
            if Ast.List.Length not in 3 .. 4 then
               raise Argument_Error with "if: expects 2 or 3 arguments";
            end if;
            declare
               Test : constant Mal.T := Eval (Ast.List.Element (2), Env);
            begin
               if (case Test.Kind is
                   when Kind_Nil => False,
                   when Kind_Boolean => Test.Ada_Boolean,
                   when others => True)
               then
                  return Eval (Ast.List.Element (3), Env);
               elsif Ast.List.Length = 3 then
                  return Mal.Nil;
               else
                  return Eval (Ast.List.Element (4), Env);
               end if;
            end;
         elsif First.Symbol = Symbols.Names.Let then
            if Ast.List.Length /= 3 then
               raise Argument_Error with "let*: expects 3 arguments";
            elsif Ast.List.Element (2).Kind not in Kind_List | Kind_Vector then
               raise Argument_Error with "let*: expects a list or vector";
            end if;
            declare
               Bindings : constant Lists.Ptr := Ast.List.Element (2).List;
               --  This curious syntax is useful for later steps.
               New_Env  : Envs.Ptr := Env.Copy_Pointer;
            begin
               New_Env.Replace_With_Sub;
               if Bindings.Length mod 2 /= 0 then
                  raise Argument_Error with "let*: odd number of bindings";
               end if;
               for I in 1 .. Bindings.Length / 2 loop
                  if Bindings.Element (2 * I - 1).Kind /= Kind_Symbol then
                     raise Argument_Error with "let*: keys must be symbols";
                  end if;
                  New_Env.Set (Bindings.Element (2 * I - 1).Symbol,
                               Eval (Bindings.Element (2 * I), New_Env));
               end loop;
               return Eval (Ast.List.Element (3), New_Env);
            end;
         else
            First := Eval (First, Env);
         end if;
      when others =>
         First := Eval (First, Env);
      end case;

      --  Apply phase.
      --  Ast is a non-empty list,
      --  First is its non-special evaluated first element.
      case First.Kind is
         when Kind_Builtin =>
            declare
               Args : Mal.T_Array (2 .. Ast.List.Length);
            begin
               for I in Args'Range loop
                  Args (I) := Eval (Ast.List.Element (I), Env);
               end loop;
               return First.Builtin.all (Args);
            end;
         when Kind_Function =>
            declare
               Args : Mal.T_Array (2 .. Ast.List.Length);
            begin
               for I in Args'Range loop
                  Args (I) := Eval (Ast.List.Element (I), Env);
               end loop;
               return First.Fn.Apply (Args);
            end;
         when others =>
            raise Argument_Error with "cannot call " & Printer.Img (First);
      end case;
   end Eval;

   procedure Eval_P (Ast : in Mal.T;
                     Env : in Envs.Ptr)
   is
      Result : constant Mal.T := Eval (Ast, Env);
   begin
      pragma Unreferenced (Result);
   end Eval_P;

   procedure Print (Ast : in Mal.T) is
   begin
      Ada.Text_IO.Unbounded_IO.Put_Line (Printer.Pr_Str (Ast));
   end Print;

   function Read return Mal.T is (Reader.Read_Str (Readline.Input ("user> ")));

   procedure Rep (Env : in Envs.Ptr) is
   begin
      Print (Eval (Read, Env));
   end Rep;

   ----------------------------------------------------------------------

   Startup : constant String := "(do "
     & "(def! not (fn* (a) (if a false true)))"
     & ")";
   Repl : Envs.Ptr renames Envs.Repl;
begin
   --  Show the Eval function to other packages.
   Eval_Cb.Cb := Eval'Unrestricted_Access;
   --  Add Core functions into the top environment.
   for Binding of Core.Ns loop
      Repl.Set (Binding.Symbol, (Kind_Builtin, Binding.Builtin));
   end loop;
   --  Native startup procedure.
   Eval_P (Reader.Read_Str (Startup), Repl);
   loop
      begin
         Rep (Repl);
      exception
         when Readline.End_Of_File =>
            exit;
         when Reader.Empty_Source =>
            null;
         when E : Argument_Error | Reader.Reader_Error | Envs.Unknown_Key =>
            Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
         --  Other exceptions are unexpected.
      end;
   end loop;
   Ada.Text_IO.New_Line;
end Step4_If_Fn_Do;
