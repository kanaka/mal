with Ada.Command_Line;
with Ada.Text_IO.Unbounded_IO;

with Core;
with Envs;
with Err;
with Garbage_Collected;
with Printer;
with Reader;
with Readline;
with Types.Fns;
with Types.Maps;
with Types.Sequences;
with Types.Strings;

procedure Step9_Try is

   Dbgeval : constant Types.String_Ptr := Types.Strings.Alloc ("DEBUG-EVAL");

   use type Types.T;
   use all type Types.Kind_Type;
   use type Types.Strings.Instance;
   package ACL renames Ada.Command_Line;

   function Read return Types.T_Array with Inline;

   function Eval (Ast0 : in Types.T;
                  Env0 : in Envs.Ptr) return Types.T;
   function Eval_Builtin (Args : in Types.T_Array) return Types.T;
   --  The built-in variant needs to see the Repl variable.

   function Quasiquote (Ast : in Types.T) return Types.T;

   procedure Print (Ast : in Types.T) with Inline;

   procedure Rep (Env : in Envs.Ptr) with Inline;

   function Eval_Map (Source : in Types.Maps.Instance;
                      Env    : in Envs.Ptr) return Types.T;
   function Eval_Vector (Source : in Types.Sequences.Instance;
                         Env    : in Envs.Ptr) return Types.T;
   --  Helpers for the Eval function.

   procedure Exec (Script : in String;
                   Env    : in Envs.Ptr) with Inline;
   --  Read the script, eval its elements, but ignore the result.

   ----------------------------------------------------------------------

   function Eval (Ast0 : in Types.T;
                  Env0 : in Envs.Ptr) return Types.T
   is
      --  Use local variables, that can be rewritten when tail call
      --  optimization goes to <<Restart>>.
      Ast            : Types.T  := Ast0;
      Env            : Envs.Ptr := Env0;
      First          : Types.T;
   begin
      <<Restart>>
      if Types.To_Boolean (Env.all.Get_Or_Nil (Dbgeval)) then
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
         if First.Str.all = "if" then
            Err.Check (Ast.Sequence.all.Length in 3 .. 4,
                       "expected 2 or 3 parameters");
            if Types.To_Boolean (Eval (Ast.Sequence.all.Data (2), Env)) then
               Ast := Ast.Sequence.all.Data (3);
               goto Restart;
            elsif Ast.Sequence.all.Length = 3 then
               return Types.Nil;
            else
               Ast := Ast.Sequence.all.Data (4);
               goto Restart;
            end if;
         elsif First.Str.all = "let*" then
            Err.Check (Ast.Sequence.all.Length = 3
               and then Ast.Sequence.all.Data (2).Kind in Types.Kind_Sequence,
                       "expected a sequence then a value");
            declare
               Bindings : Types.T_Array
                 renames Ast.Sequence.all.Data (2).Sequence.all.Data;
            begin
               Err.Check (Bindings'Length mod 2 = 0, "expected even binds");
               Env := Envs.New_Env (Outer => Env);
               for I in 0 .. Bindings'Length / 2 - 1 loop
                  Env.all.Set (Bindings (Bindings'First + 2 * I),
                         Eval (Bindings (Bindings'First + 2 * I + 1), Env));
                  --  This call checks key kind.
               end loop;
               Ast := Ast.Sequence.all.Data (3);
               goto Restart;
            end;
         elsif First.Str.all = "quote" then
            Err.Check (Ast.Sequence.all.Length = 2, "expected 1 parameter");
            return Ast.Sequence.all.Data (2);
         elsif First.Str.all = "def!" then
            Err.Check (Ast.Sequence.all.Length = 3, "expected 2 parameters");
            declare
               Key : Types.T renames Ast.Sequence.all.Data (2);
               Val : constant Types.T := Eval (Ast.Sequence.all.Data (3), Env);
            begin
               Env.all.Set (Key, Val); --  Check key kind.
               return Val;
            end;
         elsif First.Str.all = "defmacro!" then
            Err.Check (Ast.Sequence.all.Length = 3, "expected 2 parameters");
            declare
               Key : Types.T renames Ast.Sequence.all.Data (2);
               Fun : constant Types.T := Eval (Ast.Sequence.all.Data (3), Env);
               Val : Types.T;
            begin
               Err.Check (Fun.Kind = Kind_Fn, "expected a function");
               Val := (Kind_Macro, Types.Fns.New_Function
                         (Params   => Fun.Fn.all.Params,
                          Ast      => Fun.Fn.all.Ast,
                          Env      => Fun.Fn.all.Env));
               Env.all.Set (Key, Val);  --  Check key kind.
               return Val;
            end;
         elsif First.Str.all = "do" then
            Err.Check (1 < Ast.Sequence.all.Length, "do expects arguments");
            declare
               Result : Types.T;
            begin
               for I in 2 .. Ast.Sequence.all.Length - 1 loop
                  Result := Eval (Ast.Sequence.all.Data (I), Env);
               end loop;
               pragma Unreferenced (Result);
            end;
            Ast := Ast.Sequence.all.Data (Ast.Sequence.all.Length);
            goto Restart;
         elsif First.Str.all = "fn*" then
            Err.Check (Ast.Sequence.all.Length = 3, "expected 2 parameters");
            declare
               Params : Types.T renames Ast.Sequence.all.Data (2);
            begin
               Err.Check (Params.Kind in Types.Kind_Sequence,
                          "first argument of fn* must be a sequence");
               return (Kind_Fn, Types.Fns.New_Function
                 (Params => Params.Sequence,
                  Ast    => Ast.Sequence.all.Data (3),
                  Env    => Env));
            end;
         elsif First.Str.all = "quasiquote" then
            Err.Check (Ast.Sequence.all.Length = 2, "expected 1 parameter");
            Ast := Quasiquote (Ast.Sequence.all.Data (2));
            goto Restart;
         elsif First.Str.all = "try*" then
            if Ast.Sequence.all.Length = 2 then
               Ast := Ast.Sequence.all.Data (2);
               goto Restart;
            end if;
            Err.Check (Ast.Sequence.all.Length = 3
                         and then Ast.Sequence.all.Data (3).Kind = Kind_List,
                       "expected 1 parameter, maybe followed by a list");
            declare
               A3 : Types.T_Array
                 renames Ast.Sequence.all.Data (3).Sequence.all.Data;
            begin
               Err.Check (A3'Length = 3
                            and then A3 (A3'First).Kind = Kind_Symbol
                            and then A3 (A3'First).Str.all = "catch*",
                          "3rd parameter if present must be a catch* list");
               begin
                  return Eval (Ast.Sequence.all.Data (2), Env);
               exception
                  when Err.Error =>
                     null;
               end;
               Env := Envs.New_Env (Outer => Env);
               Env.all.Set (A3 (A3'First + 1), Err.Data); --  check key kind
               Ast := A3 (A3'Last);
               goto Restart;
            end;
         else
            --  Equivalent to First := Eval (First, Env)
            --  except that we already know enough to spare a recursive call.
            First := Env.all.Get (First.Str);
         end if;
      when Kind_Nil | Kind_Atom | Kind_Boolean | Kind_Number | Types.Kind_Key
        | Kind_Macro | Types.Kind_Function =>
         --  Equivalent to First := Eval (First, Env)
         --  except that we already know enough to spare a recursive call.
         null;
      when Types.Kind_Sequence | Kind_Map =>
         --  Lists are definitely worth a recursion, and the two other
         --  cases should be rare (they will report an error later).
         First := Eval (First, Env);
      end case;

      --  Apply phase.
      --  Ast is a non-empty list,
      --  First is its non-special evaluated first element.
      case First.Kind is
      when Kind_Macro =>
         --  Use the unevaluated arguments.
         Ast := First.Fn.all.Apply
           (Ast.Sequence.all.Data (2 .. Ast.Sequence.all.Length));
         --  Then evaluate the result with TCO.
         goto Restart;
      when Types.Kind_Function =>
         null;
      when others =>
         Err.Raise_With ("first element must be a function or macro");
      end case;
      --  We are applying a function. Evaluate its arguments.
      declare
         Args : Types.T_Array (2 .. Ast.Sequence.all.Length);
      begin
         for I in Args'Range loop
            Args (I) := Eval (Ast.Sequence.all.Data (I), Env);
         end loop;
         if First.Kind = Kind_Builtin then
            return First.Builtin.all (Args);
         end if;
         --  Like Types.Fns.Apply, except that we use TCO.
         Env := Envs.New_Env (Outer => First.Fn.all.Env);
         Env.all.Set_Binds (Binds => First.Fn.all.Params.all.Data,
                            Exprs => Args);
         Ast := First.Fn.all.Ast;
         goto Restart;
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

   procedure Exec (Script : in String;
                   Env    : in Envs.Ptr)
   is
      Result : Types.T;
   begin
      for Expression of Reader.Read_Str (Script) loop
         Result := Eval (Expression, Env);
      end loop;
      pragma Unreferenced (Result);
   end Exec;

   procedure Print (Ast : in Types.T) is
   begin
      Ada.Text_IO.Unbounded_IO.Put_Line (Printer.Pr_Str (Ast));
   end Print;

   function Quasiquote (Ast : in Types.T) return Types.T is

      function Qq_Seq return Types.T;
      function Starts_With (Sequence : Types.T_Array;
                            Symbol   : String) return Boolean;

      function Qq_Seq return Types.T is
         Result : Types.T := Types.Sequences.List ((1 .. 0 => Types.Nil));
      begin
         for Elt of reverse Ast.Sequence.all.Data loop
            if Elt.Kind = Kind_List
              and then Starts_With (Elt.Sequence.all.Data, "splice-unquote")
            then
               Err.Check (Elt.Sequence.all.Length = 2,
                          "splice-unquote expects 1 parameter");
               Result := Types.Sequences.List
                 (((Kind_Symbol, Types.Strings.Alloc ("concat")),
                   Elt.Sequence.all.Data (2), Result));
            else
               Result := Types.Sequences.List
                 (((Kind_Symbol, Types.Strings.Alloc ("cons")),
                   Quasiquote (Elt), Result));
            end if;
         end loop;
         return Result;
      end Qq_Seq;

      function Starts_With (Sequence : Types.T_Array;
                            Symbol   : String) return Boolean is
         (0 < Sequence'Length
            and then Sequence (Sequence'First).Kind = Kind_Symbol
            and then Sequence (Sequence'First).Str.all = Symbol);

   begin
      case Ast.Kind is
         when Kind_List =>
            if Starts_With (Ast.Sequence.all.Data, "unquote") then
               Err.Check (Ast.Sequence.all.Length = 2, "expected 1 parameter");
               return Ast.Sequence.all.Data (2);
            else
               return Qq_Seq;
            end if;
         when Kind_Vector =>
            return Types.Sequences.List
              (((Kind_Symbol, Types.Strings.Alloc ("vec")), Qq_Seq));
         when Kind_Map | Kind_Symbol =>
            return Types.Sequences.List
              (((Kind_Symbol, Types.Strings.Alloc ("quote")), Ast));
         when others =>
            return Ast;
      end case;
   exception
      when Err.Error =>
         Err.Add_Trace_Line ("quasiquote", Ast);
         raise;
   end Quasiquote;

   function Read return Types.T_Array
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
     & "  (eval (read-string (str ""(do "" (slurp f) ""\nnil)"")))))"
     & "(defmacro! cond (fn* (& xs)"
     & "  (if (> (count xs) 0)"
     & "    (list 'if (first xs)"
     & "      (if (> (count xs) 1) (nth xs 1)"
     & "        (throw ""odd number of forms to cond""))"
     & "      (cons 'cond (rest (rest xs)))))))";
   Repl : constant Envs.Ptr := Envs.New_Env;
   function Eval_Builtin (Args : in Types.T_Array) return Types.T is
   begin
      Err.Check (Args'Length = 1, "expected 1 parameter");
      return Eval (Args (Args'First), Repl);
   end Eval_Builtin;
   Script : constant Boolean := 0 < ACL.Argument_Count;
   Argv   : constant Types.Sequence_Ptr
     := Types.Sequences.Constructor (Integer'Max (0, ACL.Argument_Count - 1));
begin
   --  Show the Eval function to other packages.
   Types.Fns.Eval_Cb := Eval'Unrestricted_Access;
   --  Add Core functions into the top environment.
   Core.NS_Add_To_Repl (Repl);
   Repl.all.Set ((Kind_Symbol, Types.Strings.Alloc ("eval")),
                 (Kind_Builtin, Eval_Builtin'Unrestricted_Access));
   --  Native startup procedure.
   Exec (Startup, Repl);
   --  Define ARGV from command line arguments.
   for I in 2 .. ACL.Argument_Count loop
      Argv.all.Data (I - 1) := (Kind_String,
                                Types.Strings.Alloc (ACL.Argument (I)));
   end loop;
   Repl.all.Set ((Kind_Symbol, Types.Strings.Alloc ("*ARGV*")),
                 (Kind_List, Argv));
   --  Execute user commands.
   if Script then
      Exec ("(load-file """ & ACL.Argument (1) & """)", Repl);
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

         --  Collect garbage.
         Err.Data := Types.Nil;
         Repl.all.Keep;
         Dbgeval.Keep;
         Garbage_Collected.Clean;
      end loop;
      Ada.Text_IO.New_Line;
   end if;

   --  If assertions are enabled, check deallocations.
   --  Normal runs do not need to deallocate before termination.
   --  Beware that all pointers are now dangling.
   pragma Debug (Garbage_Collected.Clean);
   Garbage_Collected.Check_Allocations;
end Step9_Try;
