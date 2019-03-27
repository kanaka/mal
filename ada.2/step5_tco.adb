with Ada.Environment_Variables;
with Ada.Text_IO.Unbounded_IO;

with Core;
with Envs;
with Err;
with Eval_Cb;
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

procedure Step5_Tco is

   Dbgenv1 : constant Boolean := Ada.Environment_Variables.Exists ("dbgenv1");
   Dbgenv0 : constant Boolean
     := Dbgenv1 or Ada.Environment_Variables.Exists ("dbgenv0");
   Dbgeval : constant Boolean
     := Dbgenv0 or Ada.Environment_Variables.Exists ("dbgeval");

   use Types;
   use type Mal.T;

   function Read return Mal.T_Array with Inline;

   function Eval (Ast0 : in Mal.T;
                  Env0 : in Envs.Ptr) return Mal.T;

   procedure Print (Ast : in Mal.T) with Inline;

   procedure Rep (Env : in Envs.Ptr) with Inline;

   function Eval_Seq_Elts is new Sequences.Generic_Eval (Envs.Ptr, Eval);
   function Eval_Map_Elts is new Maps.Generic_Eval (Envs.Ptr, Eval);

   procedure Exec (Script : in String;
                   Env    : in Envs.Ptr) with Inline;
   --  Read the script, eval its elements, but ignore the result.

   ----------------------------------------------------------------------

   function Eval (Ast0 : in Mal.T;
                  Env0 : in Envs.Ptr) return Mal.T
   is
      use type Symbols.Ptr;
      --  Use local variables, that can be rewritten when tail call
      --  optimization goes to <<Restart>>.
      Ast            : Mal.T    := Ast0;
      Env            : Envs.Ptr := Env0.Copy_Pointer;
      First          : Mal.T;
   begin
      <<Restart>>
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
         --  do is a built-in function, shortening this test cascade.
         elsif First.Symbol = Symbols.Names.Fn then
            Err.Check (Ast.Sequence.Length = 3, "expected 2 parameters");
            Err.Check (Ast.Sequence (2).Kind in Kind_Sequence,
                       "parameter 1 must be a sequence");
            return Fns.New_Function (Params => Ast.Sequence (2).Sequence,
                                     Ast    => Ast.Sequence (3),
                                     Env    => Env.New_Closure);
         elsif First.Symbol = Symbols.Names.Mal_If then
            Err.Check (Ast.Sequence.Length in 3 .. 4,
                       "expected 2 or 3 parameters");
            declare
               Test : constant Mal.T := Eval (Ast.Sequence (2), Env);
            begin
               if Test /= Mal.Nil and Test /= (Kind_Boolean, False) then
                  Ast := Ast.Sequence (3);
                  goto Restart;
               elsif Ast.Sequence.Length = 3 then
                  return Mal.Nil;
               else
                  Ast := Ast.Sequence (4);
                  goto Restart;
               end if;
            end;
         elsif First.Symbol = Symbols.Names.Let then
            Err.Check (Ast.Sequence.Length = 3, "expected 2 parameters");
            Err.Check (Ast.Sequence (2).Kind in Kind_Sequence,
                       "parameter 1 must be a sequence");
            declare
               Bindings : constant Sequences.Ptr := Ast.Sequence (2).Sequence;
            begin
               Err.Check (Bindings.Length mod 2 = 0,
                          "parameter 1 must have an even length");
               Env.Replace_With_Sub;
               for I in 1 .. Bindings.Length / 2 loop
                  Err.Check (Bindings (2 * I - 1).Kind = Kind_Symbol,
                             "binding keys must be symbols");
                  Env.Set (Bindings (2 * I - 1).Symbol,
                           Eval (Bindings (2 * I), Env));
               end loop;
               Ast := Ast.Sequence (3);
               goto Restart;
            end;
         else
            --  Equivalent to First := Eval (First, Env)
            --  except that we already know enough to spare a recursive call.
            First := Env.Get (First.Symbol);
         end if;
      when Kind_Nil | Kind_Atom | Kind_Boolean | Kind_Number | Kind_Key
        | Kind_Macro | Kind_Function =>
         --  Equivalent to First := Eval (First, Env)
         --  except that we already know enough to spare a recursive call.
         null;
      when Kind_Sequence | Kind_Map =>
         --  Lists are definitely worth a recursion, and the two other
         --  cases should be rare (they will report an error later).
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
         when Kind_Fn =>
            declare
               Args : Mal.T_Array (2 .. Ast.Sequence.Length);
            begin
               for I in Args'Range loop
                  Args (I) := Eval (Ast.Sequence (I), Env);
               end loop;
               Env.Replace_With_Sub (Outer => First.Fn.Env,
                                     Binds => First.Fn.Params,
                                     Exprs => Args);
               Ast := First.Fn.Ast;
               goto Restart;
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
   Repl : Envs.Ptr renames Envs.Repl;
begin
   --  Show the Eval function to other packages.
   Eval_Cb.Cb := Eval'Unrestricted_Access;
   --  Add Core functions into the top environment.
   Core.NS_Add_To_Repl;
   --  Native startup procedure.
   Exec (Startup, Repl);
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
end Step5_Tco;
