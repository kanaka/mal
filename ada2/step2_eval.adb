with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Exceptions;
with Ada.Strings.Hash;
with Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO;
with Interfaces.C.Strings; use type Interfaces.C.Strings.chars_ptr;
with Atoms;
with Lists;
with Printer;
with Reader;
with Types;

procedure Step2_Eval is

   package Environments is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => String,
      Element_Type    => Types.Native_Function_Access,
      Hash            => Ada.Strings.Hash,
      Equivalent_Keys => "=",
      "="             => Types."=");

   function Read (Source : in String) return Types.Mal_Type
     renames Reader.Read_Str;

   function Eval (Ast : in     Types.Mal_Type;
                  Env : in out Environments.Map) return Types.Mal_Type;
   Unable_To_Call : exception;
   Unknown_Symbol : exception;

   function Print (Ast            : in Types.Mal_Type;
                   Print_Readably : in Boolean        := True)
                  return Ada.Strings.Unbounded.Unbounded_String
     renames Printer.Pr_Str;

   function Rep (Source : in     String;
                 Env    : in out Environments.Map)
                return Ada.Strings.Unbounded.Unbounded_String
   is (Print (Eval (Read (Source), Env)))
     with Inline;

   procedure Interactive_Loop (Repl : in out Environments.Map)
     with Inline;

   generic
      with function Ada_Operator (Left, Right : in Integer) return Integer;
   function Generic_Mal_Operator (Args : in Types.Mal_Type_Array)
                                 return Types.Mal_Type;

   ----------------------------------------------------------------------

   function Eval (Ast : in     Types.Mal_Type;
                  Env : in out Environments.Map) return Types.Mal_Type
   is
      use Types;
   begin
      case Ast.Kind is
      when Kind_Nil | Kind_Atom | Kind_Boolean | Kind_Number | Kind_String
        | Kind_Keyword | Kind_Macro | Kind_Function | Kind_Native =>
         return Ast;

      when Kind_Symbol =>
         declare
            S : constant String := Ast.S.Deref;
            C : constant Environments.Cursor := Env.Find (S);
         begin
            if Environments.Has_Element (C) then
               return (Kind_Native, Atoms.No_Element,
                       Environments.Element (C));
            else
               raise Unknown_Symbol with "'" & S & "' not found";
            end if;
         end;

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

         --  Apply phase
         declare
            First : constant Mal_Type := Eval (Ast.L.Element (1), Env);
            Args  : Mal_Type_Array (2 .. Ast.L.Length);
         begin
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

   procedure Interactive_Loop (Repl : in out Environments.Map)
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

   Repl : Environments.Map;
begin
   Repl.Include ("+", Addition   'Unrestricted_Access);
   Repl.Include ("-", Subtraction'Unrestricted_Access);
   Repl.Include ("*", Product    'Unrestricted_Access);
   Repl.Include ("/", Division   'Unrestricted_Access);

   Interactive_Loop (Repl);
   pragma Unreferenced (Repl);
end Step2_Eval;
