with Ada.Command_Line;
with Ada.Containers.Vectors;
with Ada.Environment_Variables;
with Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO;

with Core;
with Envs;
with Err;
with Eval_Cb;
with Garbage_Collected;
with Printer;
with Reader;
with Readline;
with Types.Builtins;
with Types.Fns;
with Types.Mal;
with Types.Maps;
with Types.Sequences;
with Types.Symbols.Names;

procedure StepA_Mal is

   Dbgeval : constant Boolean := Ada.Environment_Variables.Exists ("dbgeval");

   use Types;
   use type Mal.T;
   package ACL renames Ada.Command_Line;
   package ASU renames Ada.Strings.Unbounded;

   function Read return Mal.T_Array with Inline;

   function Eval (Ast0 : in Mal.T;
                  Env0 : in Envs.Ptr) return Mal.T;
   function Eval_Builtin (Args : in Mal.T_Array) return Mal.T;
   --  The built-in variant needs to see the Repl variable.

   function Quasiquote (Ast : in Mal.T;
                        Env : in Envs.Ptr) return Mal.T;
   --  Mergeing quote and quasiquote into eval with a flag triggering
   --  a different behaviour as done for macros in step8 would improve
   --  the performances significantly, but Kanaka finds that it breaks
   --  too much the step structure shared by all implementations.

   procedure Print (Ast : in Mal.T) with Inline;

   procedure Rep (Env : in Envs.Ptr) with Inline;

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
      Env            : Envs.Ptr := Env0;
      Macroexpanding : Boolean  := False;
      First          : Mal.T;
   begin
      <<Restart>>
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
         elsif First.Symbol = Symbols.Names.Defmacro then
            Err.Check (Ast.Sequence.all.Length = 3, "expected 2 parameters");
            Err.Check (Ast.Sequence.all (2).Kind = Kind_Symbol,
                       "parameter 1 must be a symbol");
            declare
               F : constant Mal.T  := Eval (Ast.Sequence.all (3), Env);
            begin
               Err.Check (F.Kind = Kind_Fn, "parameter 2 must be a function");
               return R : constant Mal.T := F.Fn.all.New_Macro do
                  Env.all.Set (Ast.Sequence.all (2).Symbol, R);
               end return;
            end;
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
                  Ast := Ast.Sequence.all (3);
                  goto Restart;
               elsif Ast.Sequence.all.Length = 3 then
                  return Mal.Nil;
               else
                  Ast := Ast.Sequence.all (4);
                  goto Restart;
               end if;
            end;
         elsif First.Symbol = Symbols.Names.Let then
            Err.Check (Ast.Sequence.all.Length = 3, "expected 2 parameters");
            Err.Check (Ast.Sequence.all (2).Kind in Kind_Sequence,
                       "parameter 1 must be a sequence");
            declare
               Bindings : constant Mal.Sequence_Ptr
                 := Ast.Sequence.all (2).Sequence;
            begin
               Err.Check (Bindings.all.Length mod 2 = 0,
                          "parameter 1 must have an even length");
               Env := Envs.New_Env (Outer => Env);
               for I in 1 .. Bindings.all.Length / 2 loop
                  Err.Check (Bindings.all (2 * I - 1).Kind = Kind_Symbol,
                             "binding keys must be symbols");
                  Env.all.Set (Bindings.all (2 * I - 1).Symbol,
                               Eval (Bindings.all (2 * I), Env));
               end loop;
               Ast := Ast.Sequence.all (3);
               goto Restart;
            end;
         elsif First.Symbol = Symbols.Names.Macroexpand then
            Err.Check (Ast.Sequence.all.Length = 2, "expected 1 parameter");
            Macroexpanding := True;
            Ast := Ast.Sequence.all (2);
            goto Restart;
         elsif First.Symbol = Symbols.Names.Quasiquote then
            Err.Check (Ast.Sequence.all.Length = 2, "expected 1 parameter");
            return Quasiquote (Ast.Sequence.all (2), Env);
         elsif First.Symbol = Symbols.Names.Quote then
            Err.Check (Ast.Sequence.all.Length = 2, "expected 1 parameter");
            return Ast.Sequence.all (2);
         elsif First.Symbol = Symbols.Names.Try then
            if Ast.Sequence.all.Length = 2 then
               Ast := Ast.Sequence.all (2);
               goto Restart;
            end if;
            Err.Check (Ast.Sequence.all.Length = 3,
                       "expected 1 or 2 parameters");
            Err.Check (Ast.Sequence.all (3).Kind = Kind_List,
                       "parameter 2 must be a list");
            declare
               A3 : constant Mal.Sequence_Ptr := Ast.Sequence.all (3).Sequence;
            begin
               Err.Check (A3.all.Length = 3,
                          "length of parameter 2 must be 3");
               Err.Check (A3.all (1) = (Kind_Symbol, Symbols.Names.Catch),
                          "parameter 3 must start with 'catch*'");
               Err.Check (A3.all (2).Kind = Kind_Symbol,
                          "a symbol must follow catch*");
               begin
                  return Eval (Ast.Sequence.all (2), Env);
               exception
                  when Err.Error =>
                     null;
               end;
               Env := Envs.New_Env (Outer => Env);
               Env.all.Set (A3.all (2).Symbol, Err.Data);
               Ast := A3.all (3);
               goto Restart;
            end;
         else
            --  Equivalent to First := Eval (First, Env)
            --  except that we already know enough to spare a recursive call.
            First := Env.all.Get (First.Symbol);
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
               Args : Mal.T_Array (2 .. Ast.Sequence.all.Length);
            begin
               for I in Args'Range loop
                  Args (I) := Eval (Ast.Sequence.all (I), Env);
               end loop;
               return First.Builtin.all (Args);
            end;
         when Kind_Builtin_With_Meta =>
            declare
               Args : Mal.T_Array (2 .. Ast.Sequence.all.Length);
            begin
               for I in Args'Range loop
                  Args (I) := Eval (Ast.Sequence.all (I), Env);
               end loop;
               return First.Builtin_With_Meta.all.Builtin.all (Args);
            end;
         when Kind_Fn =>
            declare
               Args : Mal.T_Array (2 .. Ast.Sequence.all.Length);
            begin
               for I in Args'Range loop
                  Args (I) := Eval (Ast.Sequence.all (I), Env);
               end loop;
               Env := Envs.New_Env (Outer => First.Fn.all.Env,
                                    Binds => First.Fn.all.Params,
                                    Exprs => Args);
               Ast := First.Fn.all.Ast;
               goto Restart;
            end;
         when Kind_Macro =>
            declare
               Args : constant Mal.T_Array
                 := Ast.Sequence.all.Tail (Ast.Sequence.all.Length - 1);
            begin
               if Macroexpanding then
                  --  Evaluate the macro with tail call optimization.
                  Env := Envs.New_Env (Outer => Env,
                                       Binds => First.Fn.all.Params,
                                       Exprs => Args);
                  Ast := First.Fn.all.Ast;
                  goto Restart;
               else
                  --  Evaluate the macro normally.
                  Ast := Eval (First.Fn.all.Ast,
                               Envs.New_Env (Outer => Env,
                                             Binds => First.Fn.all.Params,
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

      function Quasiquote_List (List : in Sequences.Instance) return Mal.T
        with Inline;
      --  Handle vectors and lists not starting with unquote.

      function Quasiquote_List (List : in Sequences.Instance) return Mal.T is
         package Vectors is new Ada.Containers.Vectors (Positive, Mal.T);
         Vector   : Vectors.Vector; --  buffer for concatenation
         Sequence : Mal.Sequence_Ptr;
         Tmp      : Mal.T;
      begin
         for I in 1 .. List.Length loop
            if List (I).Kind in Kind_List
              and then 0 < List (I).Sequence.all.Length
              and then List (I).Sequence.all (1)
                = (Kind_Symbol, Symbols.Names.Splice_Unquote)
            then
               Err.Check (List (I).Sequence.all.Length = 2,
                          "splice-unquote expects 1 parameter");
               Tmp := Eval (List (I).Sequence.all (2), Env);
               Err.Check (Tmp.Kind = Kind_List,
                          "splice_unquote expects a list");
               for I in 1 .. Tmp.Sequence.all.Length loop
                  Vector.Append (Tmp.Sequence.all (I));
               end loop;
            else
               Vector.Append (Quasiquote (List (I), Env));
            end if;
         end loop;
         --  Now that we know the number of elements, convert to a list.
         Sequence := Sequences.Constructor (Natural (Vector.Length));
         for I in 1 .. Natural (Vector.Length) loop
            Sequence.Replace_Element (I, Vector (I));
         end loop;
         return (Kind_List, Sequence);
      end Quasiquote_List;

   begin                                --  Quasiquote
      case Ast.Kind is
         when Kind_Vector =>
            --  When the test is updated, replace Kind_List with Kind_Vector.
            return Quasiquote_List (Ast.Sequence.all);
         when Kind_List =>
            if 0 < Ast.Sequence.all.Length
              and then Ast.Sequence.all (1) = (Kind_Symbol,
                                               Symbols.Names.Unquote)
            then
               Err.Check (Ast.Sequence.all.Length = 2, "expected 1 parameter");
               return Eval (Ast.Sequence.all (2), Env);
            else
               return Quasiquote_List (Ast.Sequence.all);
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
     & "(def! *gensym-counter* (atom 0))"
     & "(def! gensym (fn* [] "
     & "  (symbol (str ""G__"" (swap! *gensym-counter* (fn* [x] (+ 1 x)))))))"
     & "(defmacro! or (fn* (& xs)"
     & "  (if (empty? xs) nil"
     & "  (if (= 1 (count xs)) (first xs)"
     & "  (let* (condvar (gensym))"
     & "    `(let* (~condvar ~(first xs))"
     & "      (if ~condvar ~condvar (or ~@(rest xs)))))))))"
     & "(def! *host-language* ""ada.2"")";
   Repl : constant Envs.Ptr := Envs.New_Env;
   function Eval_Builtin (Args : in Mal.T_Array) return Mal.T is
   begin
      Err.Check (Args'Length = 1, "expected 1 parameter");
      return Eval_Cb.Cb.all (Args (Args'First), Repl);
   end Eval_Builtin;
   Script : constant Boolean := 0 < ACL.Argument_Count;
   Argv   : Mal.Sequence_Ptr;
begin
   --  Show the Eval function to other packages.
   Eval_Cb.Cb := Eval'Unrestricted_Access;
   --  Add Core functions into the top environment.
   Core.NS_Add_To_Repl (Repl);
   Repl.all.Set (Symbols.Constructor ("eval"),
                 (Kind_Builtin, Eval_Builtin'Unrestricted_Access));
   --  Native startup procedure.
   Exec (Startup, Repl);
   --  Define ARGV from command line arguments.
   if Script then
      Argv := Sequences.Constructor (ACL.Argument_Count - 1);
      for I in 2 .. ACL.Argument_Count loop
         Argv.all.Replace_Element
           (I - 1, (Kind_String, ASU.To_Unbounded_String (ACL.Argument (I))));
      end loop;
   else
      Argv := Sequences.Constructor (0);
   end if;
   Repl.all.Set (Symbols.Constructor ("*ARGV*"), (Kind_List, Argv));
   --  Execute user commands.
   if Script then
      Exec ("(load-file """ & ACL.Argument (1) & """)", Repl);
   else
      Exec ("(println (str ""Mal ["" *host-language* ""]""))", Repl);
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
   end if;

   --  If assertions are enabled, check deallocations.
   pragma Debug (Garbage_Collected.Clean);
   Garbage_Collected.Check_Allocations;
   Symbols.Check_Allocations;
end StepA_Mal;
