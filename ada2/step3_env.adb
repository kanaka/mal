with Ada.Exceptions;
with Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO;
with Interfaces.C.Strings;

with Environments;
with Printer;
with Reader;
with Types.Lists;
with Types.Mal;
with Types.Maps;
with Types.Symbols.Names;

procedure Step3_Env is

   package ASU renames Ada.Strings.Unbounded;
   use Types;
   use type Symbols.Ptr;

   function Read (Source : in String) return Mal.T
     renames Reader.Read_Str;

   function Eval (Ast : in Mal.T;
                  Env : in Environments.Ptr) return Mal.T;

   function Print (Ast      : in Mal.T;
                   Readably : in Boolean := True) return ASU.Unbounded_String
     renames Printer.Pr_Str;

   function Rep (Source : in String;
                 Env    : in Environments.Ptr) return ASU.Unbounded_String
   is (Print (Eval (Read (Source), Env))) with Inline;

   procedure Interactive_Loop (Repl : in Environments.Ptr);

   generic
      with function Ada_Operator (Left, Right : in Integer) return Integer;
   function Generic_Mal_Operator (Args : in Mal.T_Array) return Mal.T;

   function Eval_Elements is new Lists.Generic_Eval (Environments.Ptr, Eval);
   function Eval_Elements is new Maps.Generic_Eval (Environments.Ptr, Eval);

   ----------------------------------------------------------------------

   function Eval (Ast : in Mal.T;
                  Env : in Environments.Ptr) return Mal.T is
      First          : Mal.T;
   begin
      --  Ada.Text_IO.New_Line;
      --  Ada.Text_IO.Put ("EVAL: ");
      --  Ada.Text_IO.Unbounded_IO.Put_Line (Print (Ast));
      --  Environments.Dump_Stack;
      case Ast.Kind is
      when Kind_Symbol =>
         return Env.Get (Ast.Symbol);
      when Kind_Map =>
         return Eval_Elements (Ast.Map, Env);
      when Kind_Vector =>
         return (Kind_Vector, Eval_Elements (Ast.L, Env));
      when Kind_List =>
         if Ast.L.Length = 0 then
            return Ast;
         end if;
         First := Ast.L.Element (1);
         --  Special forms
         if First.Kind /= Kind_Symbol then
            --  Evaluate First, in the less frequent case where it is
            --  not a symbol.
            First := Eval (First, Env);
         elsif First.Symbol = Symbols.Names.Def then
            if Ast.L.Length /= 3 then
               raise Argument_Error with "def!: expects 2 arguments";
            elsif Ast.L.Element (2).Kind /= Kind_Symbol then
               raise Argument_Error with "def!: arg 1 must be a symbol";
            end if;
            return R : constant Mal.T := Eval (Ast.L.Element (3), Env) do
               Env.Set (Ast.L.Element (2).Symbol, R);
            end return;
         elsif First.Symbol = Symbols.Names.Let then
            if Ast.L.Length /= 3 then
               raise Argument_Error with "let*: expects 3 arguments";
            elsif Ast.L.Element (2).Kind not in Kind_List | Kind_Vector then
               raise Argument_Error with "let*: expects a list or vector";
            end if;
            declare
               Bindings : constant Lists.Ptr := Ast.L.Element (2).L;
               New_Env : constant Environments.Ptr := Env.Sub;
            begin
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
               return Eval (Ast.L.Element (3), New_Env);
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
               Args : Mal.T_Array (2 .. Ast.L.Length);
            begin
               for I in Args'Range loop
                  Args (I) := Eval (Ast.L.Element (I), Env);
               end loop;
               return First.Builtin.all (Args);
            end;
         when others =>
            raise Argument_Error
              with "cannot execute " & ASU.To_String (Print (First));
         end case;
      when others =>
         return Ast;
      end case;
   end Eval;

   function Generic_Mal_Operator (Args : in Mal.T_Array) return Mal.T
   is (Kind_Number, Ada_Operator (Args (Args'First).Ada_Number,
                                  Args (Args'Last).Ada_Number));

   procedure Interactive_Loop (Repl : in Environments.Ptr) is
      use Interfaces.C, Interfaces.C.Strings;
      function Readline (Prompt : in char_array) return chars_ptr
        with Import, Convention => C, External_Name => "readline";
      procedure Add_History (Line : in chars_ptr)
        with Import, Convention => C, External_Name => "add_history";
      procedure Free (Line : in chars_ptr)
        with Import, Convention => C, External_Name => "free";
      Prompt : constant char_array := To_C ("user> ");
      C_Line : chars_ptr;
   begin
      loop
         C_Line := Readline (Prompt);
         exit when C_Line = Null_Ptr;
         declare
            Line : constant String := Value (C_Line);
         begin
            if Line /= "" then
               Add_History (C_Line);
            end if;
            Free (C_Line);
            Ada.Text_IO.Unbounded_IO.Put_Line (Rep (Line, Repl));
         exception
            when Reader.Empty_Source =>
               null;
            when E : Argument_Error | Reader.Reader_Error
              | Environments.Unknown_Key =>
               Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
            --  Other exceptions are unexpected.
         end;
      end loop;
      Ada.Text_IO.New_Line;
   end Interactive_Loop;

   ----------------------------------------------------------------------

   function Addition    is new Generic_Mal_Operator ("+");
   function Subtraction is new Generic_Mal_Operator ("-");
   function Product     is new Generic_Mal_Operator ("*");
   function Division    is new Generic_Mal_Operator ("/");

   function S (Source : in String) return Symbols.Ptr
     renames Symbols.Constructor;
   Repl : Environments.Ptr renames Environments.Repl;
begin
   Repl.Set (S ("+"), (Kind_Builtin, Addition   'Unrestricted_Access));
   Repl.Set (S ("-"), (Kind_Builtin, Subtraction'Unrestricted_Access));
   Repl.Set (S ("*"), (Kind_Builtin, Product    'Unrestricted_Access));
   Repl.Set (S ("/"), (Kind_Builtin, Division   'Unrestricted_Access));

   Interactive_Loop (Repl);
end Step3_Env;
