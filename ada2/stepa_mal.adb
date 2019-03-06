with Ada.Command_Line;
with Ada.Exceptions;
with Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO;

with Core;
with Envs;
with Eval_Cb;
with Printer;
with Reader;
with Readline;
with Types.Functions;
with Types.Lists;
with Types.Mal;
with Types.Maps;
with Types.Symbols.Names;

procedure StepA_Mal is

   package ASU renames Ada.Strings.Unbounded;
   use Types;

   function Read return Mal.T with Inline;

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

   function Eval_List_Elts is new Lists.Generic_Eval (Envs.Ptr, Eval);
   function Eval_Map_Elts  is new Maps.Generic_Eval  (Envs.Ptr, Eval);

   --  Procedural form of Eval.
   --  Convenient when the result of eval is of no interest.
   procedure Eval_P (Ast : in Mal.T;
                     Env : in Envs.Ptr) with Inline;

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
      --  Ada.Text_IO.New_Line;
      --  Ada.Text_IO.Put ("EVAL: ");
      --  Ada.Text_IO.Unbounded_IO.Put_Line (Print (Ast));
      --  Envs.Dump_Stack;
      case Ast.Kind is
      when Kind_Nil | Kind_Atom | Kind_Boolean | Kind_Number | Kind_String
        | Kind_Keyword | Kind_Macro | Kind_Function
        | Kind_Builtin_With_Meta | Kind_Builtin =>
         return Ast;
      when Kind_Symbol =>
         return Env.Get (Ast.Symbol);
      when Kind_Map =>
         return Eval_Map_Elts (Ast.Map, Env);
      when Kind_Vector =>
         return (Kind_Vector, Eval_List_Elts (Ast.List, Env));
      when Kind_List =>
         if Ast.List.Length = 0 then
            return Ast;
         end if;
         First := Ast.List.Element (1);
         --  Special forms
         if First.Kind /= Kind_Symbol then
            --  Evaluate First, in the less frequent case where it is
            --  not a symbol.
            First := Eval (First, Env);
         elsif First.Symbol = Symbols.Names.Def then
            if Ast.List.Length /= 3 then
               raise Argument_Error with "def!: expects 2 arguments";
            elsif Ast.List.Element (2).Kind /= Kind_Symbol then
               raise Argument_Error with "def!: arg 1 must be a symbol";
            end if;
            return R : constant Mal.T := Eval (Ast.List.Element (3), Env) do
               Env.Set (Ast.List.Element (2).Symbol, R);
            end return;
         elsif First.Symbol = Symbols.Names.Defmacro then
            if Ast.List.Length /= 3 then
               raise Argument_Error with "defmacro!: expects 2 arguments";
            elsif Ast.List.Element (2).Kind /= Kind_Symbol then
               raise Argument_Error with "defmacro!: arg 1 must be a symbol";
            end if;
            declare
               F : constant Mal.T  := Eval (Ast.List.Element (3), Env);
            begin
               if F.Kind /= Kind_Function then
                  raise Argument_Error with "defmacro!: expects a function";
               end if;
               return R : constant Mal.T := F.Fn.New_Macro do
                  Env.Set (Ast.List.Element (2).Symbol, R);
               end return;
            end;
         elsif First.Symbol = Symbols.Names.Mal_Do then
            if Ast.List.Length = 1 then
               raise Argument_Error with "do: expects at least 1 argument";
            end if;
            for I in 2 .. Ast.List.Length - 1 loop
               Eval_P (Ast.List.Element (I), Env);
            end loop;
            Ast := Ast.List.Element (Ast.List.Length);
            goto Restart;
         elsif First.Symbol = Symbols.Names.Fn then
            if Ast.List.Length /= 3 then
               raise Argument_Error with "fn*: expects 3 arguments";
            elsif Ast.List.Element (2).Kind not in Kind_List | Kind_Vector then
               raise Argument_Error with "fn*: arg 1 must be a list or vector";
            elsif (for some F in 1 .. Ast.List.Element (2).List.Length =>
                     Ast.List.Element (2).List.Element (F).Kind /= Kind_Symbol)
            then
               raise Argument_Error with "fn*: arg 2 must contain symbols";
            end if;
            return Functions.New_Function (Params => Ast.List.Element (2).List,
                                           Ast    => Ast.List.Element (3),
                                           Env    => Env.New_Closure);
         elsif First.Symbol = Symbols.Names.Mal_If then
            if Ast.List.Length not in 3 .. 4 then
               raise Argument_Error with "if: expects 2 or 3 arguments";
            end if;
            declare
               Test : constant Mal.T := Eval (Ast.List.Element (2), Env);
            begin
               if (case Test.Kind is
                   when Kind_Nil => False,
                   when Kind_Boolean => Test.Ada_Boolean,
                   when others => True)
               then
                  Ast := Ast.List.Element (3);
                  goto Restart;
               elsif Ast.List.Length = 3 then
                  return Mal.Nil;
               else
                  Ast := Ast.List.Element (4);
                  goto Restart;
               end if;
            end;
         elsif First.Symbol = Symbols.Names.Let then
            if Ast.List.Length /= 3 then
               raise Argument_Error with "let*: expects 3 arguments";
            elsif Ast.List.Element (2).Kind not in Kind_List | Kind_Vector then
               raise Argument_Error with "let*: expects a list or vector";
            end if;
            declare
               Bindings : constant Lists.Ptr := Ast.List.Element (2).List;
            begin
               if Bindings.Length mod 2 /= 0 then
                  raise Argument_Error with "let*: odd number of bindings";
               end if;
               Env.Replace_With_Sub;
               for I in 1 .. Bindings.Length / 2 loop
                  if Bindings.Element (2 * I - 1).Kind /= Kind_Symbol then
                     raise Argument_Error with "let*: keys must be symbols";
                  end if;
                  Env.Set (Bindings.Element (2 * I - 1).Symbol,
                           Eval (Bindings.Element (2 * I), Env));
               end loop;
               Ast := Ast.List.Element (3);
               goto Restart;
            end;
         elsif First.Symbol = Symbols.Names.Macroexpand then
            if Ast.List.Length /= 2 then
               raise Argument_Error with "macroexpand: expects 1 argument";
            end if;
            Macroexpanding := True;
            Ast := Ast.List.Element (2);
            goto Restart;
         elsif First.Symbol = Symbols.Names.Quasiquote then
            if Ast.List.Length /= 2 then
               raise Argument_Error with "quasiquote: expects 1 argument";
            end if;
            return Quasiquote (Ast.List.Element (2), Env);
         elsif First.Symbol = Symbols.Names.Quote then
            if Ast.List.Length /= 2 then
               raise Argument_Error with "quote: expects 1 argument";
            end if;
            return Ast.List.Element (2);
         elsif First.Symbol = Symbols.Names.Try then
            if Ast.List.Length = 2 then
               Ast := Ast.List.Element (2);
               goto Restart;
            elsif Ast.List.Length /= 3 then
               raise Argument_Error with "try*: expects 1 or 2 arguments";
            elsif Ast.List.Element (3).Kind /= Kind_List then
               raise Argument_Error with "try*: argument 2 must be a list";
            end if;
            declare
               A3 : constant Lists.Ptr := Ast.List.Element (3).List;
            begin
               if A3.Length /= 3 then
                  raise Argument_Error with "try*: arg 2 must have 3 elements";
               elsif A3.Element (1).Kind /= Kind_Symbol
                 or else A3.Element (1).Symbol /= Symbols.Names.Catch
               then
                  raise Argument_Error with "try*: arg 2 must be a catch*";
               elsif A3.Element (2).Kind /= Kind_Symbol then
                  raise Argument_Error with "catch*: expects a symbol";
               end if;
               begin
                  return Eval (Ast.List.Element (2), Env);
               exception
                  when E : Reader.Empty_Source | Argument_Error
                    | Reader.Reader_Error | Envs.Unknown_Key =>
                     Env.Replace_With_Sub;
                     Env.Set (A3.Element (2).Symbol,
                              Mal.T'(Kind_String, ASU.To_Unbounded_String
                                 (Ada.Exceptions.Exception_Message (E))));
                     Ast := A3.Element (3);
                     goto Restart;
                  when Core.Exception_Throwed =>
                     Env.Replace_With_Sub;
                     Env.Set (A3.Element (2).Symbol, Core.Last_Exception);
                     Ast := A3.Element (3);
                     goto Restart;
                  --  Other exceptions are unexpected.
               end;
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
               Args : Mal.T_Array (2 .. Ast.List.Length);
            begin
               for I in Args'Range loop
                  Args (I) := Eval (Ast.List.Element (I), Env);
               end loop;
               return First.Builtin.all (Args);
            end;
         when Kind_Builtin_With_Meta =>
            declare
               Args : Mal.T_Array (2 .. Ast.List.Length);
            begin
               for I in Args'Range loop
                  Args (I) := Eval (Ast.List.Element (I), Env);
               end loop;
               return First.Builtin_With_Meta.Builtin.all (Args);
            end;
         when Kind_Function =>
            declare
               Args : Mal.T_Array (2 .. Ast.List.Length);
            begin
               for I in Args'Range loop
                  Args (I) := Eval (Ast.List.Element (I), Env);
               end loop;
               Env.Replace_With_Sub (Outer => First.Fn.Env,
                                     Binds => First.Fn.Params,
                                     Exprs => Args);
               Ast := First.Fn.Ast;
               goto Restart;
            end;
         when Kind_Macro =>
            Ast := Eval (Ast0 => First.Fn.Ast,
                         Env0 => Envs.Sub (Outer => Env,
                                           Binds => First.Fn.Params,
                                           Exprs => Ast.List));
            if Macroexpanding then
               return Ast;
            else
               goto Restart;
            end if;
         when others =>
            raise Argument_Error with "cannot call " & Printer.Img (First);
         end case;
      end case;
   end Eval;

   procedure Eval_P (Ast : in Mal.T;
                     Env : in Envs.Ptr)
   is
      Result : constant Mal.T := Eval (Ast, Env);
   begin
      pragma Unreferenced (Result);
   end Eval_P;

   procedure Print (Ast : in Mal.T) is
   begin
      Ada.Text_IO.Unbounded_IO.Put_Line (Printer.Pr_Str (Ast));
   end Print;

   function Quasiquote (Ast : in Mal.T;
                        Env : in Envs.Ptr) return Mal.T
   is

      use type Symbols.Ptr;

      function Quasiquote_List (List : in Lists.Ptr) return Mal.T with Inline;
      --  Handle vectors and lists not starting with unquote.

      function Quasiquote_List (List : in Lists.Ptr) return Mal.T is
         --  The final return concatenates these lists.
         R : Mal.T_Array (1 .. List.Length);
      begin
         for I in R'Range loop
            R (I) := List.Element (I);
            if R (I).Kind in Kind_List | Kind_Vector
              and then 0 < R (I).List.Length
              and then R (I).List.Element (1).Kind = Kind_Symbol
              and then R (I).List.Element (1).Symbol
                       = Symbols.Names.Splice_Unquote
            then
               if R (I).List.Length /= 2 then
                  raise Argument_Error with "splice-unquote: expects 1 arg";
               end if;
               R (I) := Eval (R (I).List.Element (2), Env);
               if R (I).Kind /= Kind_List then
                  raise Argument_Error with "splice-unquote: expects a list";
               end if;
            else
               R (I) := Lists.List (Mal.T_Array'(1 => Quasiquote (R (I),
                                                                  Env)));
            end if;
         end loop;
         return Lists.Concat (R);
      end Quasiquote_List;

   begin                                --  Quasiquote
      case Ast.Kind is
         when Kind_Vector =>
            --  When the test is updated, replace Kind_List with Kind_Vector.
            return Quasiquote_List (Ast.List);
         when Kind_List =>
            if 0 < Ast.List.Length
              and then Ast.List.Element (1).Kind = Kind_Symbol
              and then Ast.List.Element (1).Symbol = Symbols.Names.Unquote
            then
               return Eval (Ast.List.Element (2), Env);
            else
               return Quasiquote_List (Ast.List);
            end if;
         when others =>
            return Ast;
      end case;
   end Quasiquote;

   function Read return Mal.T is (Reader.Read_Str (Readline.Input ("user> ")));

   procedure Rep (Env : in Envs.Ptr) is
   begin
      Print (Eval (Read, Env));
   end Rep;

   ----------------------------------------------------------------------

   Startup : constant String := "(do "
     & "(def! not (fn* (a) (if a false true)))"
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
     & "(def! *host-language* ""ada2"")"
     & ")";
   Repl : Envs.Ptr renames Envs.Repl;
   use Ada.Command_Line;
begin
   --  Show the Eval function to other packages.
   Eval_Cb.Cb := Eval'Unrestricted_Access;
   --  Add Core functions into the top environment.
   for Binding of Core.Ns loop
      Repl.Set (Binding.Symbol, (Kind_Builtin, Binding.Builtin));
   end loop;
   --  Native startup procedure.
   Eval_P (Reader.Read_Str (Startup), Repl);
   --  Define ARGV from command line arguments.
   declare
      Args : Mal.T_Array (2 .. Argument_Count);
   begin
      for I in Args'Range loop
         Args (I) := (Kind_String, ASU.To_Unbounded_String (Argument (I)));
      end loop;
      Repl.Set (Symbols.Constructor ("*ARGV*"), Lists.List (Args));
   end;
   --  Script?
   if 0 < Argument_Count then
      Eval_P (Reader.Read_Str ("(load-file """ & Argument (1) & """)"), Repl);
   else
      Eval_P (Reader.Read_Str
                ("(println (str ""Mal ["" *host-language* ""]""))"), Repl);
      loop
         begin
            Rep (Repl);
         exception
            when Readline.End_Of_File =>
               exit;
            when Reader.Empty_Source =>
               null;
            when E : Argument_Error | Reader.Reader_Error | Envs.Unknown_Key =>
               Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
            when Core.Exception_Throwed =>
               Ada.Text_IO.Put ("User exception: ");
               Ada.Text_IO.Unbounded_IO.Put_Line (Printer.Pr_Str
                                                    (Core.Last_Exception));
            --  Other exceptions are unexpected.
         end;
      end loop;
      Ada.Text_IO.New_Line;
   end if;
end StepA_Mal;
