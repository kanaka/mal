with Ada.Environment_Variables;
with Ada.Text_IO.Unbounded_IO;

with Envs;
with Err;
with Printer;
with Reader;
with Readline;
with Types.Atoms;
with Types.Builtins;
with Types.Fns;
with Types.Mal;
with Types.Maps;
with Types.Sequences;
with Types.Symbols.Names;

procedure Step3_Env is

   Dbgenv1 : constant Boolean := Ada.Environment_Variables.Exists ("dbgenv1");
   Dbgenv0 : constant Boolean
     := Dbgenv1 or Ada.Environment_Variables.Exists ("dbgenv0");
   Dbgeval : constant Boolean
     := Dbgenv0 or Ada.Environment_Variables.Exists ("dbgeval");

   use Types;

   function Read return Mal.T_Array with Inline;

   function Eval (Ast : in Mal.T;
                  Env : in Envs.Ptr) return Mal.T;

   procedure Print (Ast : in Mal.T) with Inline;

   procedure Rep (Env : in Envs.Ptr) with Inline;

   generic
      with function Ada_Operator (Left, Right : in Integer) return Integer;
   function Generic_Mal_Operator (Args : in Mal.T_Array) return Mal.T;

   function Eval_Seq_Elts is new Sequences.Generic_Eval (Envs.Ptr, Eval);
   function Eval_Map_Elts is new Maps.Generic_Eval (Envs.Ptr, Eval);

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
         if Dbgenv0 then
            Envs.Dump_Stack (Long => Dbgenv1);
         end if;
      end if;
      case Ast.Kind is
      when Kind_Nil | Kind_Atom | Kind_Boolean | Kind_Number | Kind_Key
        | Kind_Macro | Kind_Function =>
         return Ast;
      when Kind_Symbol =>
         return Env.Get (Ast.Symbol);
      when Kind_Map =>
         return Eval_Map_Elts (Ast.Map, Env);
      when Kind_Vector =>
         return (Kind_Vector, Eval_Seq_Elts (Ast.Sequence, Env));
      when Kind_List =>
         null;
      end case;

      --  Ast is a list.
      if Ast.Sequence.Length = 0 then
         return Ast;
      end if;
      First := Ast.Sequence (1);

      --  Special forms
      --  Ast is a non-empty list, First is its first element.
      case First.Kind is
      when Kind_Symbol =>
         if First.Symbol = Symbols.Names.Def then
            Err.Check (Ast.Sequence.Length = 3, "expected 2 parameters");
            Err.Check (Ast.Sequence (2).Kind = Kind_Symbol,
                       "parameter 1 must be a symbol");
            return R : constant Mal.T := Eval (Ast.Sequence (3), Env) do
               Env.Set (Ast.Sequence (2).Symbol, R);
            end return;
         elsif First.Symbol = Symbols.Names.Let then
            Err.Check (Ast.Sequence.Length = 3, "expected 2 parameters");
            Err.Check (Ast.Sequence (2).Kind in Kind_Sequence,
                       "parameter 1 must be a sequence");
            declare
               Bindings : constant Sequences.Ptr := Ast.Sequence (2).Sequence;
               --  This curious syntax is useful for later steps.
               New_Env  : Envs.Ptr := Env.Copy_Pointer;
            begin
               Err.Check (Bindings.Length mod 2 = 0,
                          "parameter 1 must have an even length");
               New_Env.Replace_With_Sub;
               for I in 1 .. Bindings.Length / 2 loop
                  Err.Check (Bindings (2 * I - 1).Kind = Kind_Symbol,
                             "binding keys must be symbols");
                  New_Env.Set (Bindings (2 * I - 1).Symbol,
                               Eval (Bindings (2 * I), New_Env));
               end loop;
               return Eval (Ast.Sequence (3), New_Env);
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
               Args : Mal.T_Array (2 .. Ast.Sequence.Length);
            begin
               for I in Args'Range loop
                  Args (I) := Eval (Ast.Sequence (I), Env);
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
         when Err.Error =>
            Ada.Text_IO.Unbounded_IO.Put (Err.Trace);
      end;
      --  Other exceptions are really unexpected.
   end loop;
   Ada.Text_IO.New_Line;
   --  If assertions are enabled, check deallocations.
   Err.Data := Mal.Nil;  --  Remove references to other packages
   pragma Debug (Envs.Clear_And_Check_Allocations);
   pragma Debug (Atoms.Check_Allocations);
   pragma Debug (Builtins.Check_Allocations);
   pragma Debug (Fns.Check_Allocations);
   pragma Debug (Maps.Check_Allocations);
   pragma Debug (Sequences.Check_Allocations);
   pragma Debug (Symbols.Check_Allocations);
end Step3_Env;
