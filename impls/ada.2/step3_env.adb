with Ada.Environment_Variables;
with Ada.Text_IO.Unbounded_IO;

with Envs;
with Err;
with Garbage_Collected;
with Printer;
with Reader;
with Readline;
with Types.Maps;
with Types.Sequences;
with Types.Strings;

procedure Step3_Env is

   Dbgeval : constant Boolean := Ada.Environment_Variables.Exists ("dbgeval");

   use type Types.T;
   use all type Types.Kind_Type;
   use type Types.Strings.Instance;

   function Read return Types.T_Array with Inline;

   function Eval (Ast : in Types.T;
                  Env : in Envs.Ptr) return Types.T;

   procedure Print (Ast : in Types.T) with Inline;

   procedure Rep (Env : in Envs.Ptr) with Inline;

   generic
      with function Ada_Operator (Left, Right : in Integer) return Integer;
   function Generic_Mal_Operator (Args : in Types.T_Array) return Types.T;

   function Eval_Map (Source : in Types.Maps.Instance;
                      Env    : in Envs.Ptr) return Types.T;
   function Eval_Vector (Source : in Types.Sequences.Instance;
                         Env    : in Envs.Ptr) return Types.T;
   --  Helpers for the Eval function.

   ----------------------------------------------------------------------

   function Eval (Ast : in Types.T;
                  Env : in Envs.Ptr) return Types.T
   is
      First          : Types.T;
   begin
      if Dbgeval then
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put ("EVAL: ");
         Print (Ast);
         Envs.Dump_Stack (Env.all);
      end if;

      case Ast.Kind is
      when Kind_Nil | Kind_Atom | Kind_Boolean | Kind_Number | Types.Kind_Key
        | Kind_Macro | Types.Kind_Function =>
         return Ast;
      when Kind_Symbol =>
         return Env.all.Get (Ast.Str);
      when Kind_Map =>
         return Eval_Map (Ast.Map.all, Env);
      when Kind_Vector =>
         return Eval_Vector (Ast.Sequence.all, Env);
      when Kind_List =>
         null;
      end case;

      --  Ast is a list.
      if Ast.Sequence.all.Length = 0 then
         return Ast;
      end if;
      First := Ast.Sequence.all.Data (1);

      --  Special forms
      --  Ast is a non-empty list, First is its first element.
      case First.Kind is
      when Kind_Symbol =>
         if First.Str.all = "let*" then
            Err.Check (Ast.Sequence.all.Length = 3
               and then Ast.Sequence.all.Data (2).Kind in Types.Kind_Sequence,
                       "expected a sequence then a value");
            declare
               Bindings : Types.T_Array
                 renames Ast.Sequence.all.Data (2).Sequence.all.Data;
               New_Env  : constant Envs.Ptr := Envs.New_Env (Outer => Env);
            begin
               Err.Check (Bindings'Length mod 2 = 0, "expected even binds");
               for I in 0 .. Bindings'Length / 2 - 1 loop
                  New_Env.all.Set (Bindings (Bindings'First + 2 * I),
                     Eval (Bindings (Bindings'First + 2 * I + 1), New_Env));
                  --  This call checks key kind.
               end loop;
               return Eval (Ast.Sequence.all.Data (3), New_Env);
            end;
         elsif First.Str.all = "def!" then
            Err.Check (Ast.Sequence.all.Length = 3, "expected 2 parameters");
            declare
               Key : Types.T renames Ast.Sequence.all.Data (2);
               Val : constant Types.T := Eval (Ast.Sequence.all.Data (3), Env);
            begin
               Env.all.Set (Key, Val); --  Check key kind.
               return Val;
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
      Err.Check (First.Kind = Kind_Builtin,
                 "first element must be a function");
      --  We are applying a function. Evaluate its arguments.
      declare
         Args : Types.T_Array (2 .. Ast.Sequence.all.Length);
      begin
         for I in Args'Range loop
            Args (I) := Eval (Ast.Sequence.all.Data (I), Env);
         end loop;
         return First.Builtin.all (Args);
      end;
   exception
      when Err.Error =>
         Err.Add_Trace_Line ("eval", Ast);
         raise;
   end Eval;

   function Eval_Map (Source : in Types.Maps.Instance;
                      Env    : in Envs.Ptr) return Types.T
   is
      use all type Types.Maps.Cursor;
      --  Copy the whole map so that keys are not hashed again.
      Result   : constant Types.T  := Types.Maps.New_Map (Source);
      Position : Types.Maps.Cursor := Result.Map.all.First;
   begin
      while Has_Element (Position) loop
         Result.Map.all.Replace_Element (Position,
                                         Eval (Element (Position), Env));
         Next (Position);
      end loop;
      return Result;
   end Eval_Map;

   function Eval_Vector (Source : in Types.Sequences.Instance;
                         Env    : in Envs.Ptr) return Types.T
   is
      Ref : constant Types.Sequence_Ptr
        := Types.Sequences.Constructor (Source.Length);
   begin
      for I in Source.Data'Range loop
         Ref.all.Data (I) := Eval (Source.Data (I), Env);
      end loop;
      return (Kind_Vector, Ref);
   end Eval_Vector;

   function Generic_Mal_Operator (Args : in Types.T_Array) return Types.T
   is (Kind_Number, Ada_Operator (Args (Args'First).Number,
                                  Args (Args'Last).Number));

   procedure Print (Ast : in Types.T) is
   begin
      Ada.Text_IO.Unbounded_IO.Put_Line (Printer.Pr_Str (Ast));
   end Print;

   function Read return Types.T_Array
   is (Reader.Read_Str (Readline.Input ("user> ")));

   procedure Rep (Env : in Envs.Ptr) is
   begin
      for Expression of Read loop
         Print (Eval (Expression, Env));
      end loop;
   end Rep;

   ----------------------------------------------------------------------

   function Addition    is new Generic_Mal_Operator ("+");
   function Subtraction is new Generic_Mal_Operator ("-");
   function Product     is new Generic_Mal_Operator ("*");
   function Division    is new Generic_Mal_Operator ("/");

   Repl   : constant Envs.Ptr := Envs.New_Env;
begin
   --  Add Core functions into the top environment.
   Repl.all.Set ((Kind_Symbol, Types.Strings.Alloc ("+")),
                 (Kind_Builtin, Addition   'Unrestricted_Access));
   Repl.all.Set ((Kind_Symbol, Types.Strings.Alloc ("-")),
                 (Kind_Builtin, Subtraction'Unrestricted_Access));
   Repl.all.Set ((Kind_Symbol, Types.Strings.Alloc ("*")),
                 (Kind_Builtin, Product    'Unrestricted_Access));
   Repl.all.Set ((Kind_Symbol, Types.Strings.Alloc ("/")),
                 (Kind_Builtin, Division   'Unrestricted_Access));
   --  Execute user commands.
   loop
      begin
         Rep (Repl);
      exception
         when Readline.End_Of_File =>
            exit;
         when Err.Error =>
            Ada.Text_IO.Unbounded_IO.Put (Err.Trace);
      end;
      --  Other exceptions are really unexpected.

      --  Collect garbage.
      Err.Data := Types.Nil;
      Repl.all.Keep;
      Garbage_Collected.Clean;
   end loop;
   Ada.Text_IO.New_Line;

   --  If assertions are enabled, check deallocations.
   --  Normal runs do not need to deallocate before termination.
   --  Beware that all pointers are now dangling.
   pragma Debug (Garbage_Collected.Clean);
   Garbage_Collected.Check_Allocations;
end Step3_Env;
