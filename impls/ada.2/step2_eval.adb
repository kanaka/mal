with Ada.Environment_Variables;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;
with Ada.Text_IO.Unbounded_IO;

with Err;
with Garbage_Collected;
with Printer;
with Reader;
with Readline;
with Types.Maps;
with Types.Sequences;
with Types.Strings;

procedure Step2_Eval is

   Dbgeval : constant Boolean := Ada.Environment_Variables.Exists ("dbgeval");

   use type Types.T;
   use all type Types.Kind_Type;

   package Envs is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => String,
      Element_Type    => Types.Builtin_Ptr,
      Hash            => Ada.Strings.Hash,
      Equivalent_Keys => "=",
      "="             => Types."=");

   function Read return Types.T_Array with Inline;

   function Eval (Ast : in Types.T;
                  Env : in Envs.Map) return Types.T;

   procedure Print (Ast : in Types.T) with Inline;

   procedure Rep (Env : in Envs.Map) with Inline;

   generic
      with function Ada_Operator (Left, Right : in Integer) return Integer;
   function Generic_Mal_Operator (Args : in Types.T_Array) return Types.T;

   function Eval_Map (Source : in Types.Maps.Instance;
                      Env    : in Envs.Map) return Types.T;
   function Eval_Vector (Source : in Types.Sequences.Instance;
                         Env    : in Envs.Map) return Types.T;
   --  Helpers for the Eval function.

   ----------------------------------------------------------------------

   function Eval (Ast : in Types.T;
                  Env : in Envs.Map) return Types.T
   is
      First          : Types.T;
   begin
      if Dbgeval then
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put ("EVAL: ");
         Print (Ast);
      end if;

      case Ast.Kind is
      when Kind_Nil | Kind_Atom | Kind_Boolean | Kind_Number | Types.Kind_Key
        | Kind_Macro | Types.Kind_Function =>
         return Ast;
      when Kind_Symbol =>
         declare
            S : constant String      := Ast.Str.all.To_String;
            C : constant Envs.Cursor := Env.Find (S);
         begin
            --  The predefined error message does not pass tests.
            Err.Check (Envs.Has_Element (C), "'" & S & "' not found");
            return (Kind_Builtin, Envs.Element (C));
         end;
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

      --  Ast is a non-empty list, First is its first element.
      First := Eval (First, Env);

      --  Apply phase.
      --  Ast is a non-empty list,
      --  First is its evaluated first element.
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
                      Env    : in Envs.Map) return Types.T
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
                         Env    : in Envs.Map) return Types.T
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

   procedure Rep (Env : in Envs.Map) is
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

   Repl : Envs.Map;
begin
   Repl.Insert ("+", Addition   'Unrestricted_Access);
   Repl.Insert ("-", Subtraction'Unrestricted_Access);
   Repl.Insert ("*", Product    'Unrestricted_Access);
   Repl.Insert ("/", Division   'Unrestricted_Access);
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
      --  No data survives at this stage, Repl only contains static
      --  pointers to built-in functions.
      Garbage_Collected.Clean;
   end loop;
   Ada.Text_IO.New_Line;

   --  If assertions are enabled, check deallocations.
   --  Normal runs do not need to deallocate before termination.
   --  Beware that all pointers are now dangling.
   pragma Debug (Garbage_Collected.Clean);
   Garbage_Collected.Check_Allocations;
end Step2_Eval;
