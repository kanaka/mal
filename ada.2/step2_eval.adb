with Ada.Environment_Variables;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;
with Ada.Text_IO.Unbounded_IO;

with Err;
with Garbage_Collected;
with Printer;
with Reader;
with Readline;
with Types.Mal;
with Types.Maps;
with Types.Sequences;
with Types.Symbols;

procedure Step2_Eval is

   Dbgeval : constant Boolean := Ada.Environment_Variables.Exists ("dbgeval");

   use Types;

   package Envs is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => String,
      Element_Type    => Mal.Builtin_Ptr,
      Hash            => Ada.Strings.Hash,
      Equivalent_Keys => "=",
      "="             => Mal."=");

   function Read return Mal.T_Array with Inline;

   function Eval (Ast : in Mal.T;
                  Env : in Envs.Map) return Mal.T;

   procedure Print (Ast : in Mal.T) with Inline;

   procedure Rep (Env : in Envs.Map) with Inline;

   generic
      with function Ada_Operator (Left, Right : in Integer) return Integer;
   function Generic_Mal_Operator (Args : in Mal.T_Array) return Mal.T;

   function Eval_Map_Elts is new Maps.Generic_Eval (Envs.Map, Eval);

   ----------------------------------------------------------------------

   function Eval (Ast : in Mal.T;
                  Env : in Envs.Map) return Mal.T
   is
      First          : Mal.T;
   begin
      if Dbgeval then
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put ("EVAL: ");
         Print (Ast);
      end if;

      case Ast.Kind is
      when Kind_Nil | Kind_Atom | Kind_Boolean | Kind_Number | Kind_Key
        | Kind_Macro | Kind_Function =>
         return Ast;
      when Kind_Symbol =>
         declare
            S : constant String      := Ast.Symbol.To_String;
            C : constant Envs.Cursor := Env.Find (S);
         begin
            --  The predefined error message does not pass tests.
            Err.Check (Envs.Has_Element (C), "'" & S & "' not found");
            return (Kind_Builtin, Envs.Element (C));
         end;
      when Kind_Map =>
         return Eval_Map_Elts (Ast.Map.all, Env);
      when Kind_Vector =>
         declare
            Len  : constant Natural := Ast.Sequence.all.Length;
            List : constant Mal.Sequence_Ptr := Sequences.Constructor (Len);
         begin
            for I in 1 .. Len loop
               List.all.Replace_Element (I, Eval (Ast.Sequence.all (I), Env));
            end loop;
            return (Kind_Vector, List);
         end;
      when Kind_List =>
         null;
      end case;

      --  Ast is a list.
      if Ast.Sequence.all.Length = 0 then
         return Ast;
      end if;
      First := Ast.Sequence.all (1);

      --  Ast is a non-empty list, First is its first element.
      First := Eval (First, Env);

      --  Apply phase.
      --  Ast is a non-empty list,
      --  First is its evaluated first element.
      case First.Kind is
         when Kind_Builtin =>
            declare
               Args : Mal.T_Array (2 .. Ast.Sequence.all.Length);
            begin
               for I in Args'Range loop
                  Args (I) := Eval (Ast.Sequence.all (I), Env);
               end loop;
               return First.Builtin.all (Args);
            end;
         when others =>
            Err.Raise_With ("first element must be a function");
      end case;
   exception
      when Err.Error =>
         Err.Add_Trace_Line ("eval", Ast);
         raise;
   end Eval;

   function Generic_Mal_Operator (Args : in Mal.T_Array) return Mal.T
   is (Kind_Number, Ada_Operator (Args (Args'First).Number,
                                  Args (Args'Last).Number));

   procedure Print (Ast : in Mal.T) is
   begin
      Ada.Text_IO.Unbounded_IO.Put_Line (Printer.Pr_Str (Ast));
   end Print;

   function Read return Mal.T_Array
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
      Err.Data := Mal.Nil;
      Garbage_Collected.Clean;
   end loop;
   Ada.Text_IO.New_Line;

   --  If assertions are enabled, check deallocations.
   pragma Debug (Garbage_Collected.Clean);
   Garbage_Collected.Check_Allocations;
   Symbols.Check_Allocations;
end Step2_Eval;
