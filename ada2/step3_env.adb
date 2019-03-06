with Ada.Exceptions;
with Ada.Text_IO.Unbounded_IO;

with Envs;
with Printer;
with Reader;
with Readline;
with Types.Lists;
with Types.Mal;
with Types.Maps;
with Types.Symbols.Names;

procedure Step3_Env is

   use Types;

   function Read return Mal.T with Inline;

   function Eval (Ast : in Mal.T;
                  Env : in Envs.Ptr) return Mal.T;

   procedure Print (Ast : in Mal.T) with Inline;

   procedure Rep (Env : in Envs.Ptr) with Inline;

   generic
      with function Ada_Operator (Left, Right : in Integer) return Integer;
   function Generic_Mal_Operator (Args : in Mal.T_Array) return Mal.T;

   function Eval_List_Elts is new Lists.Generic_Eval (Envs.Ptr, Eval);
   function Eval_Map_Elts  is new Maps.Generic_Eval  (Envs.Ptr, Eval);

   ----------------------------------------------------------------------

   function Eval (Ast : in Mal.T;
                  Env : in Envs.Ptr) return Mal.T
   is
      use type Symbols.Ptr;
      First          : Mal.T;
   begin
      --  Ada.Text_IO.New_Line;
      --  Ada.Text_IO.Put ("EVAL: ");
      --  Ada.Text_IO.Unbounded_IO.Put_Line (Print (Ast));
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
         if Ast.List.Length = 0 then
            return Ast;
         end if;
         First := Ast.List.Element (1);
         --  Special forms
         if First.Kind /= Kind_Symbol then
            --  Evaluate First, in the less frequent case where it is
            --  not a symbol.
            First := Eval (First, Env);
         elsif First.Symbol = Symbols.Names.Def then
            if Ast.List.Length /= 3 then
               raise Argument_Error with "def!: expects 2 arguments";
            elsif Ast.List.Element (2).Kind /= Kind_Symbol then
               raise Argument_Error with "def!: arg 1 must be a symbol";
            end if;
            return R : constant Mal.T := Eval (Ast.List.Element (3), Env) do
               Env.Set (Ast.List.Element (2).Symbol, R);
            end return;
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
            --  Equivalent to First := Eval (First, Env), except that
            --  we already know enough to spare a recursive call in
            --  this frequent case.
            First := Env.Get (First.Symbol);
         end if;
         --  Apply phase.
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
         when others =>
            raise Argument_Error with "cannot call " & Printer.Img (First);
         end case;
      end case;
   end Eval;

   function Generic_Mal_Operator (Args : in Mal.T_Array) return Mal.T
   is (Kind_Number, Ada_Operator (Args (Args'First).Number,
                                  Args (Args'Last).Number));

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

   function Addition    is new Generic_Mal_Operator ("+");
   function Subtraction is new Generic_Mal_Operator ("-");
   function Product     is new Generic_Mal_Operator ("*");
   function Division    is new Generic_Mal_Operator ("/");

   Repl : Envs.Ptr renames Envs.Repl;
begin
   Repl.Set (Symbols.Constructor ("+"),
             (Kind_Builtin, Addition   'Unrestricted_Access));
   Repl.Set (Symbols.Constructor ("-"),
             (Kind_Builtin, Subtraction'Unrestricted_Access));
   Repl.Set (Symbols.Constructor ("*"),
             (Kind_Builtin, Product    'Unrestricted_Access));
   Repl.Set (Symbols.Constructor ("/"),
             (Kind_Builtin, Division   'Unrestricted_Access));
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
end Step3_Env;
