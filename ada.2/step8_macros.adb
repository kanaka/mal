with Ada.Command_Line;
with Ada.Environment_Variables;
with Ada.Strings.Unbounded;
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

procedure Step8_Macros is

   Dbgenv1 : constant Boolean := Ada.Environment_Variables.Exists ("dbgenv1");
   Dbgenv0 : constant Boolean
     := Dbgenv1 or Ada.Environment_Variables.Exists ("dbgenv0");
   Dbgeval : constant Boolean
     := Dbgenv0 or Ada.Environment_Variables.Exists ("dbgeval");

   use Types;
   use type Mal.T;
   package ASU renames Ada.Strings.Unbounded;

   function Read return Mal.T_Array with Inline;

   function Eval (Ast0 : in Mal.T;
                  Env0 : in Envs.Ptr) return Mal.T;

   function Quasiquote (Ast : in Mal.T;
                        Env : in Envs.Ptr) return Mal.T;
   --  Mergeing quote and quasiquote into eval with a flag triggering
   --  a different behaviour as done for macros in step8 would improve
   --  the performances significantly, but Kanaka finds that it breaks
   --  too much the step structure shared by all implementations.

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
      Macroexpanding : Boolean  := False;
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
         elsif First.Symbol = Symbols.Names.Defmacro then
            Err.Check (Ast.Sequence.Length = 3, "expected 2 parameters");
            Err.Check (Ast.Sequence (2).Kind = Kind_Symbol,
                       "parameter 1 must be a symbol");
            declare
               F : constant Mal.T  := Eval (Ast.Sequence (3), Env);
            begin
               Err.Check (F.Kind = Kind_Fn, "parameter 2 must be a function");
               return R : constant Mal.T := F.Fn.New_Macro do
                  Env.Set (Ast.Sequence (2).Symbol, R);
               end return;
            end;
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
         elsif First.Symbol = Symbols.Names.Macroexpand then
            Err.Check (Ast.Sequence.Length = 2, "expected 1 parameter");
            Macroexpanding := True;
            Ast := Ast.Sequence (2);
            goto Restart;
         elsif First.Symbol = Symbols.Names.Quasiquote then
            Err.Check (Ast.Sequence.Length = 2, "expected 1 parameter");
            return Quasiquote (Ast.Sequence (2), Env);
         elsif First.Symbol = Symbols.Names.Quote then
            Err.Check (Ast.Sequence.Length = 2, "expected 1 parameter");
            return Ast.Sequence (2);
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
         when Kind_Macro =>
            declare
               Args : constant Mal.T_Array
                 := Ast.Sequence.Tail (Ast.Sequence.Length - 1);
            begin
               if Macroexpanding then
                  --  Evaluate the macro with tail call optimization.
                  Env.Replace_With_Sub (Binds => First.Fn.Params,
                                        Exprs => Args);
                  Ast := First.Fn.Ast;
                  goto Restart;
               else
                  --  Evaluate the macro normally.
                  Ast := Eval (First.Fn.Ast, Envs.Sub
                                 (Outer => Env,
                                  Binds => First.Fn.Params,
                                  Exprs => Args));
                  --  Then evaluate the result with TCO.
                  goto Restart;
               end if;
            end;
         when others =>
            Err.Raise_With ("first element must be a function or macro");
      end case;
   exception
      when Err.Error =>
         if Macroexpanding then
            Err.Add_Trace_Line ("macroexpand", Ast);
         else
            Err.Add_Trace_Line ("eval", Ast);
         end if;
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

   function Quasiquote (Ast : in Mal.T;
                        Env : in Envs.Ptr) return Mal.T
   is

      function Quasiquote_List (List : in Sequences.Ptr) return Mal.T
        with Inline;
      --  Handle vectors and lists not starting with unquote.

      function Quasiquote_List (List : in Sequences.Ptr) return Mal.T is
         --  The final return concatenates these lists.
         R : Mal.T_Array (1 .. List.Length);
      begin
         for I in R'Range loop
            R (I) := List (I);
            if R (I).Kind in Kind_List and then 0 < R (I).Sequence.Length
              and then R (I).Sequence (1) = (Kind_Symbol,
                                         Symbols.Names.Splice_Unquote)
            then
               Err.Check (R (I).Sequence.Length = 2,
                          "splice-unquote expects 1 parameter");
               R (I) := Eval (@.Sequence (2), Env);
               Err.Check (R (I).Kind = Kind_List,
                          "splice_unquote expects a list");
            else
               R (I) := Sequences.List
                 (Mal.T_Array'(1 => Quasiquote (@, Env)));
            end if;
         end loop;
         return Sequences.Concat (R);
      end Quasiquote_List;

   begin                                --  Quasiquote
      case Ast.Kind is
         when Kind_Vector =>
            --  When the test is updated, replace Kind_List with Kind_Vector.
            return Quasiquote_List (Ast.Sequence);
         when Kind_List =>
            if 0 < Ast.Sequence.Length
              and then Ast.Sequence (1) = (Kind_Symbol, Symbols.Names.Unquote)
            then
               Err.Check (Ast.Sequence.Length = 2, "expected 1 parameter");
               return Eval (Ast.Sequence (2), Env);
            else
               return Quasiquote_List (Ast.Sequence);
            end if;
         when others =>
            return Ast;
      end case;
   exception
      when Err.Error =>
         Err.Add_Trace_Line ("quasiquote", Ast);
         raise;
   end Quasiquote;

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
     := "(def! not (fn* (a) (if a false true)))"
     & "(def! load-file (fn* (f)"
     & "  (eval (read-string (str ""(do "" (slurp f) "")"")))))"
     & "(defmacro! cond (fn* (& xs)"
     & "  (if (> (count xs) 0)"
     & "    (list 'if (first xs)"
     & "      (if (> (count xs) 1) (nth xs 1)"
     & "        (throw ""odd number of forms to cond""))"
     & "      (cons 'cond (rest (rest xs)))))))"
     & "(defmacro! or (fn* (& xs)"
     & "  (if (empty? xs) nil"
     & "  (if (= 1 (count xs)) (first xs)"
     & "  `(let* (or_FIXME ~(first xs))"
     & "      (if or_FIXME or_FIXME (or ~@(rest xs))))))))";
   Repl : Envs.Ptr renames Envs.Repl;
begin
   --  Show the Eval function to other packages.
   Eval_Cb.Cb := Eval'Unrestricted_Access;
   --  Add Core functions into the top environment.
   Core.NS_Add_To_Repl;
   --  Native startup procedure.
   Exec (Startup, Repl);
   --  Define ARGV from command line arguments.
   declare
      use Ada.Command_Line;
      Args : Mal.T_Array (2 .. Argument_Count);
   begin
      for I in Args'Range loop
         Args (I) := (Kind_String, ASU.To_Unbounded_String (Argument (I)));
      end loop;
      Repl.Set (Symbols.Constructor ("*ARGV*"), Sequences.List (Args));
   end;
   --  Script?
   if 0 < Ada.Command_Line.Argument_Count then
      Exec ("(load-file """ & Ada.Command_Line.Argument (1) & """)", Repl);
   else
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
   end if;
   --  If assertions are enabled, check deallocations.
   Err.Data := Mal.Nil;  --  Remove references to other packages
   pragma Debug (Envs.Clear_And_Check_Allocations);
   pragma Debug (Atoms.Check_Allocations);
   pragma Debug (Builtins.Check_Allocations);
   pragma Debug (Fns.Check_Allocations);
   pragma Debug (Maps.Check_Allocations);
   pragma Debug (Sequences.Check_Allocations);
   pragma Debug (Symbols.Check_Allocations);
end Step8_Macros;
