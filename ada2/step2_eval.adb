with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Exceptions;
with Ada.Strings.Hash;
with Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO;
with Interfaces.C.Strings;

with Printer;
with Reader;
with Types.Builtins;
with Types.Lists;
with Types.Mal;
with Types.Maps;

procedure Step2_Eval is

   package ASU renames Ada.Strings.Unbounded;
   use Types;

   package Environments is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => String,
      Element_Type    => Builtins.Ptr,
      Hash            => Ada.Strings.Hash,
      Equivalent_Keys => "=",
      "="             => Builtins."=");
   Unknown_Symbol : exception;

   function Read (Source : in String) return Mal.T
     renames Reader.Read_Str;

   function Eval (Ast : in Mal.T;
                  Env : in Environments.Map) return Mal.T;

   function Print (Ast      : in Mal.T;
                   Readably : in Boolean := True) return ASU.Unbounded_String
     renames Printer.Pr_Str;

   function Rep (Source : in String;
                 Env    : in Environments.Map) return ASU.Unbounded_String
   is (Print (Eval (Read (Source), Env))) with Inline;

   procedure Interactive_Loop (Repl : in Environments.Map);

   generic
      with function Ada_Operator (Left, Right : in Integer) return Integer;
   function Generic_Mal_Operator (Args : in Mal.T_Array) return Mal.T;

   function Eval_List_Elts is new Lists.Generic_Eval (Environments.Map, Eval);
   function Eval_Map_Elts  is new Maps.Generic_Eval (Environments.Map, Eval);

   ----------------------------------------------------------------------

   function Eval (Ast : in Mal.T;
                  Env : in Environments.Map) return Mal.T is
      First          : Mal.T;
   begin
      --  Ada.Text_IO.New_Line;
      --  Ada.Text_IO.Put ("EVAL: ");
      --  Ada.Text_IO.Unbounded_IO.Put_Line (Print (Ast));
      case Ast.Kind is
      when Kind_Nil | Kind_Atom | Kind_Boolean | Kind_Number | Kind_String
        | Kind_Keyword | Kind_Macro | Kind_Function
        | Kind_Builtin_With_Meta | Kind_Builtin =>
         return Ast;
      when Kind_Symbol =>
         declare
            S : constant String              := Ast.Symbol.To_String;
            C : constant Environments.Cursor := Env.Find (S);
         begin
            if Environments.Has_Element (C) then
               return (Kind_Builtin, Environments.Element (C));
            else
               --  The predefined message does not pass tests.
               raise Unknown_Symbol with "'" & S & "' not found";
            end if;
         end;
      when Kind_Map =>
         return Eval_Map_Elts (Ast.Map, Env);
      when Kind_Vector =>
         return (Kind_Vector, Eval_List_Elts (Ast.L, Env));
      when Kind_List =>
         if Ast.L.Length = 0 then
            return Ast;
         end if;
         First := Eval (Ast.L.Element (1), Env);
         --  Apply phase.
         case First.Kind is
         when Kind_Builtin =>
            declare
               Args : Mal.T_Array (2 .. Ast.L.Length);
            begin
               for I in Args'Range loop
                  Args (I) := Eval (Ast.L.Element (I), Env);
               end loop;
               return First.Builtin.all (Args);
            end;
         when others =>
            raise Argument_Error
              with "cannot execute " & ASU.To_String (Print (First));
         end case;
      end case;
   end Eval;

   function Generic_Mal_Operator (Args : in Mal.T_Array) return Mal.T
   is (Kind_Number, Ada_Operator (Args (Args'First).Ada_Number,
                                  Args (Args'Last).Ada_Number));

   procedure Interactive_Loop (Repl : in Environments.Map) is
      use Interfaces.C, Interfaces.C.Strings;
      function Readline (Prompt : in char_array) return chars_ptr
        with Import, Convention => C, External_Name => "readline";
      procedure Add_History (Line : in chars_ptr)
        with Import, Convention => C, External_Name => "add_history";
      procedure Free (Line : in chars_ptr)
        with Import, Convention => C, External_Name => "free";
      Prompt : constant char_array := To_C ("user> ");
      C_Line : chars_ptr;
   begin
      loop
         C_Line := Readline (Prompt);
         exit when C_Line = Null_Ptr;
         declare
            Line : constant String := Value (C_Line);
         begin
            if Line /= "" then
               Add_History (C_Line);
            end if;
            Free (C_Line);
            Ada.Text_IO.Unbounded_IO.Put_Line (Rep (Line, Repl));
         exception
            when Reader.Empty_Source =>
               null;
            when E : Argument_Error | Reader.Reader_Error
              | Unknown_Symbol =>
               Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
            --  Other exceptions are unexpected.
         end;
      end loop;
      Ada.Text_IO.New_Line;
   end Interactive_Loop;

   ----------------------------------------------------------------------

   function Addition    is new Generic_Mal_Operator ("+");
   function Subtraction is new Generic_Mal_Operator ("-");
   function Product     is new Generic_Mal_Operator ("*");
   function Division    is new Generic_Mal_Operator ("/");

   Repl : Environments.Map;
begin
   Repl.Insert ("+", Addition   'Unrestricted_Access);
   Repl.Insert ("-", Subtraction'Unrestricted_Access);
   Repl.Insert ("*", Product    'Unrestricted_Access);
   Repl.Insert ("/", Division   'Unrestricted_Access);

   Interactive_Loop (Repl);
end Step2_Eval;
