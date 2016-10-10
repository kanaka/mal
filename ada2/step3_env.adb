with Ada.Exceptions;
with Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO;
with Interfaces.C.Strings; use type Interfaces.C.Strings.chars_ptr;
with Atoms;
with Environments;
with Lists;
with Names;
with Printer;
with Reader;
with Strings; use type Strings.Ptr;
with Types;

procedure Step3_Env is

   function Read (Source : in String) return Types.Mal_Type
     renames Reader.Read_Str;

   function Eval (Ast : in Types.Mal_Type;
                  Env : in Environments.Ptr) return Types.Mal_Type;
   Unable_To_Call : exception;

   function Print (Ast            : in Types.Mal_Type;
                   Print_Readably : in Boolean        := True)
                  return Ada.Strings.Unbounded.Unbounded_String
     renames Printer.Pr_Str;

   function Rep (Source : in String;
                 Env    : in Environments.Ptr)
                return Ada.Strings.Unbounded.Unbounded_String
   is (Print (Eval (Read (Source), Env)))
     with Inline;

   procedure Interactive_Loop (Repl : in Environments.Ptr)
     with Inline;

   generic
      with function Ada_Operator (Left, Right : in Integer) return Integer;
   function Generic_Mal_Operator (Args : in Types.Mal_Type_Array)
                                 return Types.Mal_Type;

   ----------------------------------------------------------------------

   function Eval (Ast : in Types.Mal_Type;
                  Env : in Environments.Ptr) return Types.Mal_Type
   is
      use Types;
      First : Mal_Type;
   begin
      case Ast.Kind is
      when Kind_Nil | Kind_Atom | Kind_Boolean | Kind_Number | Kind_String
        | Kind_Keyword | Kind_Macro | Kind_Function | Kind_Native =>
         return Ast;

      when Kind_Symbol =>
         return Env.Get (Ast.S);

      when Kind_Map =>
         declare
            function F (X : Mal_Type) return Mal_Type is (Eval (X, Env));
         begin
            return (Kind_Map, Atoms.No_Element, Ast.Map.Map (F'Access));
         end;

      when Kind_Vector =>
         return R : constant Mal_Type := (Kind_Vector, Atoms.No_Element,
                                          Lists.Alloc (Ast.L.Length))
         do
            for I in 1 .. Ast.L.Length loop
               R.L.Replace_Element (I, Eval (Ast.L.Element (I), Env));
            end loop;
         end return;

      when Kind_List =>
         if Ast.L.Length = 0 then
            return Ast;
         end if;

         First := Ast.L.Element (1);

         --  Special forms
         if First.Kind = Kind_Symbol then

            if First.S = Names.Def then
               pragma Assert (Ast.L.Length = 3);
               pragma Assert (Ast.L.Element (2).Kind = Kind_Symbol);
               return R : constant Mal_Type := Eval (Ast.L.Element (3), Env) do
                  Env.Set (Ast.L.Element (2).S, R);
               end return;

            elsif First.S = Names.Let then
               declare
                  pragma Assert (Ast.L.Length = 3);
                  pragma Assert
                    (Ast.L.Element (2).Kind in Kind_List | Kind_Vector);
                  Bindings : constant Lists.Ptr := Ast.L.Element (2).L;
                  pragma Assert (Bindings.Length mod 2 = 0);
                  New_Env : constant Environments.Ptr
                    := Environments.Alloc (Outer => Env);
               begin
                  New_Env.Increase_Capacity (Bindings.Length / 2);
                  for I in 1 .. Bindings.Length / 2 loop
                     pragma Assert
                       (Bindings.Element (2 * I - 1).Kind = Kind_Symbol);
                     New_Env.Set (Bindings.Element (2 * I - 1).S,
                                  Eval (Bindings.Element (2 * I), New_Env));
                  end loop;
                  return Eval (Ast.L.Element (3), New_Env);
               end;
            end if;
         end if;

         --  No special form has been found, attempt to apply the
         --  first element to the rest of the list.
         declare
            Args : Mal_Type_Array (2 .. Ast.L.Length);
         begin
            First := Eval (First, Env);
            for I in Args'Range loop
               Args (I) := Eval (Ast.L.Element (I), Env);
            end loop;
            case First.Kind is
            when Kind_Native =>
               return First.Native.all (Args);
            when others =>
               raise Unable_To_Call
                 with Ada.Strings.Unbounded.To_String (Print (First));
            end case;
         end;
      end case;
   end Eval;

   function Generic_Mal_Operator (Args : in Types.Mal_Type_Array)
                                 return Types.Mal_Type
   is (Types.Kind_Number, Atoms.No_Element,
       Ada_Operator (Args (Args'First).Integer_Value,
                     Args (Args'First + 1).Integer_Value));

   procedure Interactive_Loop (Repl : in Environments.Ptr)
   is

      function Readline (Prompt : in Interfaces.C.char_array)
                        return Interfaces.C.Strings.chars_ptr
        with Import, Convention => C, External_Name => "readline";

      procedure Add_History (Line : in Interfaces.C.Strings.chars_ptr)
        with Import, Convention => C, External_Name => "add_history";

      procedure Free (Line : in Interfaces.C.Strings.chars_ptr)
        with Import, Convention => C, External_Name => "free";

      Prompt : constant Interfaces.C.char_array
        := Interfaces.C.To_C ("user> ");
      C_Line : Interfaces.C.Strings.chars_ptr;
   begin
      loop
         C_Line := Readline (Prompt);
         exit when C_Line = Interfaces.C.Strings.Null_Ptr;
         declare
            Line : constant String := Interfaces.C.Strings.Value (C_Line);
         begin
            if Line /= "" then
               Add_History (C_Line);
            end if;
            Free (C_Line);
            Ada.Text_IO.Unbounded_IO.Put_Line (Rep (Line, Repl));
         exception
            when Reader.Empty_Source =>
               null;
            when E : others =>
               Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
               --  but go on proceeding.
         end;
      end loop;
      Ada.Text_IO.New_Line;
   end Interactive_Loop;

   ----------------------------------------------------------------------

   function Addition    is new Generic_Mal_Operator ("+");
   function Subtraction is new Generic_Mal_Operator ("-");
   function Product     is new Generic_Mal_Operator ("*");
   function Division    is new Generic_Mal_Operator ("/");

   use Types;
   Repl : constant Environments.Ptr := Environments.Alloc;
begin
   Repl.Increase_Capacity (4);
   Repl.Set (Names.Plus, Types.Mal_Type'
      (Types.Kind_Native, Atoms.No_Element, Addition'Unrestricted_Access));
   Repl.Set (Names.Minus, Types.Mal_Type'
      (Types.Kind_Native, Atoms.No_Element, Subtraction'Unrestricted_Access));
   Repl.Set (Names.Asterisk, Types.Mal_Type'
      (Types.Kind_Native, Atoms.No_Element, Product'Unrestricted_Access));
   Repl.Set (Names.Slash, Types.Mal_Type'
      (Types.Kind_Native, Atoms.No_Element, Division'Unrestricted_Access));

   Interactive_Loop (Repl);
end Step3_Env;
