with Ada.Environment_Variables;
with Ada.Text_IO.Unbounded_IO;

with Core;
with Envs;
with Err;
with Eval_Cb;
with Garbage_Collected;
with Printer;
with Reader;
with Readline;
with Types.Fns;
with Types.Mal;
with Types.Maps;
with Types.Sequences;
with Types.Symbols.Names;

procedure Step4_If_Fn_Do is

   Dbgeval : constant Boolean := Ada.Environment_Variables.Exists ("dbgeval");

   use Types;
   use type Mal.T;

   function Read return Mal.T_Array with Inline;

   function Eval (Ast : in Mal.T;
                  Env : in Envs.Ptr) return Mal.T;

   procedure Print (Ast : in Mal.T) with Inline;

   procedure Rep (Env : in Envs.Ptr) with Inline;

   function Eval_Map_Elts is new Maps.Generic_Eval (Envs.Ptr, Eval);

   procedure Exec (Script : in String;
                   Env    : in Envs.Ptr) with Inline;
   --  Read the script, eval its elements, but ignore the result.

   ----------------------------------------------------------------------

   function Eval (Ast : in Mal.T;
                  Env : in Envs.Ptr) return Mal.T
   is
      use type Symbols.Ptr;
      First          : Mal.T;
   begin
      if Dbgeval then
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put ("EVAL: ");
         Print (Ast);
         Envs.Dump_Stack (Env.all);
      end if;

      case Ast.Kind is
      when Kind_Nil | Kind_Atom | Kind_Boolean | Kind_Number | Kind_Key
        | Kind_Macro | Kind_Function =>
         return Ast;
      when Kind_Symbol =>
         return Env.all.Get (Ast.Symbol);
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

      --  Special forms
      --  Ast is a non-empty list, First is its first element.
      case First.Kind is
      when Kind_Symbol =>
         if First.Symbol = Symbols.Names.Def then
            Err.Check (Ast.Sequence.all.Length = 3, "expected 2 parameters");
            Err.Check (Ast.Sequence.all (2).Kind = Kind_Symbol,
                       "parameter 1 must be a symbol");
            return R : constant Mal.T := Eval (Ast.Sequence.all (3), Env) do
               Env.all.Set (Ast.Sequence.all (2).Symbol, R);
            end return;
         --  do is a built-in function, shortening this test cascade.
         elsif First.Symbol = Symbols.Names.Fn then
            Err.Check (Ast.Sequence.all.Length = 3, "expected 2 parameters");
            Err.Check (Ast.Sequence.all (2).Kind in Kind_Sequence,
                       "parameter 1 must be a sequence");
            return Fns.New_Function
              (Params => Ast.Sequence.all (2).Sequence.all,
               Ast    => Ast.Sequence.all (3),
               Env    => Env);
         elsif First.Symbol = Symbols.Names.Mal_If then
            Err.Check (Ast.Sequence.all.Length in 3 .. 4,
                       "expected 2 or 3 parameters");
            declare
               Test : constant Mal.T := Eval (Ast.Sequence.all (2), Env);
            begin
               if Test /= Mal.Nil and Test /= (Kind_Boolean, False) then
                  return Eval (Ast.Sequence.all (3), Env);
               elsif Ast.Sequence.all.Length = 3 then
                  return Mal.Nil;
               else
                  return Eval (Ast.Sequence.all (4), Env);
               end if;
            end;
         elsif First.Symbol = Symbols.Names.Let then
            Err.Check (Ast.Sequence.all.Length = 3, "expected 2 parameters");
            Err.Check (Ast.Sequence.all (2).Kind in Kind_Sequence,
                       "parameter 1 must be a sequence");
            declare
               Bindings : constant Mal.Sequence_Ptr
                 := Ast.Sequence.all (2).Sequence;
               New_Env  : Envs.Ptr;
            begin
               Err.Check (Bindings.all.Length mod 2 = 0,
                          "parameter 1 must have an even length");
               New_Env := Envs.New_Env (Outer => Env);
               for I in 1 .. Bindings.all.Length / 2 loop
                  Err.Check (Bindings.all (2 * I - 1).Kind = Kind_Symbol,
                             "binding keys must be symbols");
                  New_Env.all.Set (Bindings.all (2 * I - 1).Symbol,
                                   Eval (Bindings.all (2 * I), New_Env));
               end loop;
               return Eval (Ast.Sequence.all (3), New_Env);
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
               Args : Mal.T_Array (2 .. Ast.Sequence.all.Length);
            begin
               for I in Args'Range loop
                  Args (I) := Eval (Ast.Sequence.all (I), Env);
               end loop;
               return First.Builtin.all (Args);
            end;
         when Kind_Fn =>
            declare
               Args : Mal.T_Array (2 .. Ast.Sequence.all.Length);
            begin
               for I in Args'Range loop
                  Args (I) := Eval (Ast.Sequence.all (I), Env);
               end loop;
               return First.Fn.all.Apply (Args);
            end;
         when others =>
            Err.Raise_With ("first element must be a function");
      end case;
   exception
      when Err.Error =>
         Err.Add_Trace_Line ("eval", Ast);
         raise;
   end Eval;

   procedure Exec (Script : in String;
                   Env    : in Envs.Ptr)
   is
      Result : Mal.T;
   begin
      for Expression of Reader.Read_Str (Script) loop
         Result := Eval (Expression, Env);
      end loop;
      pragma Unreferenced (Result);
   end Exec;

   procedure Print (Ast : in Mal.T) is
   begin
      Ada.Text_IO.Unbounded_IO.Put_Line (Printer.Pr_Str (Ast));
   end Print;

   function Read return Mal.T_Array
   is (Reader.Read_Str (Readline.Input ("user> ")));

   procedure Rep (Env : in Envs.Ptr) is
   begin
      for Expression of Read loop
         Print (Eval (Expression, Env));
      end loop;
   end Rep;

   ----------------------------------------------------------------------

   Startup : constant String
     := "(def! not (fn* (a) (if a false true)))";
   Repl   : constant Envs.Ptr := Envs.New_Env;
begin
   --  Show the Eval function to other packages.
   Eval_Cb.Cb := Eval'Unrestricted_Access;
   --  Add Core functions into the top environment.
   Core.NS_Add_To_Repl (Repl);
   --  Native startup procedure.
   Exec (Startup, Repl);
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
      Err.Data := Mal.Nil;
      Repl.all.Keep;
      Garbage_Collected.Clean;
   end loop;
   Ada.Text_IO.New_Line;

   --  If assertions are enabled, check deallocations.
   pragma Debug (Garbage_Collected.Clean);
   Garbage_Collected.Check_Allocations;
   Symbols.Check_Allocations;
end Step4_If_Fn_Do;
