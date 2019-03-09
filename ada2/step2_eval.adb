with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Exceptions;
with Ada.Strings.Hash;
with Ada.Text_IO.Unbounded_IO;

with Printer;
with Reader;
with Readline;
with Types.Lists;
with Types.Mal;
with Types.Maps;

procedure Step2_Eval is

   use Types;

   package Envs is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => String,
      Element_Type    => Mal.Builtin_Ptr,
      Hash            => Ada.Strings.Hash,
      Equivalent_Keys => "=",
      "="             => Mal."=");
   Unknown_Key : exception;

   function Read return Mal.T with Inline;

   function Eval (Ast : in Mal.T;
                  Env : in Envs.Map) return Mal.T;

   procedure Print (Ast : in Mal.T) with Inline;

   procedure Rep (Env : in Envs.Map) with Inline;

   generic
      with function Ada_Operator (Left, Right : in Integer) return Integer;
   function Generic_Mal_Operator (Args : in Mal.T_Array) return Mal.T;

   function Eval_List_Elts is new Lists.Generic_Eval (Envs.Map, Eval);
   function Eval_Map_Elts  is new Maps.Generic_Eval  (Envs.Map, Eval);

   ----------------------------------------------------------------------

   function Eval (Ast : in Mal.T;
                  Env : in Envs.Map) return Mal.T
   is
      First          : Mal.T;
   begin
      --  Ada.Text_IO.New_Line;
      --  Ada.Text_IO.Put ("EVAL: ");
      --  Print (Ast);
      case Ast.Kind is
      when Kind_Nil | Kind_Atom | Kind_Boolean | Kind_Number | Kind_String
        | Kind_Keyword | Kind_Macro | Kind_Function
        | Kind_Builtin_With_Meta | Kind_Builtin =>
         return Ast;
      when Kind_Symbol =>
         declare
            S : constant String      := Ast.Symbol.To_String;
            C : constant Envs.Cursor := Env.Find (S);
         begin
            if Envs.Has_Element (C) then
               return (Kind_Builtin, Envs.Element (C));
            else
               --  The predefined message does not pass tests.
               raise Unknown_Key with "'" & S & "' not found";
            end if;
         end;
      when Kind_Map =>
         return Eval_Map_Elts (Ast.Map, Env);
      when Kind_Vector =>
         return (Kind_Vector, Eval_List_Elts (Ast.List, Env));
      when Kind_List =>
         null;
      end case;

      --  Ast is a list.
      if Ast.List.Length = 0 then
         return Ast;
      end if;
      First := Eval (Ast.List.Element (1), Env);

      --  Apply phase.
      --  Ast is a non-empty list,
      --  First is its non-special evaluated first element.
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
         when others =>
            raise Argument_Error with "cannot call " & Printer.Img (First);
      end case;
   end Eval;

   function Generic_Mal_Operator (Args : in Mal.T_Array) return Mal.T
   is (Kind_Number, Ada_Operator (Args (Args'First).Number,
                                  Args (Args'Last).Number));

   procedure Print (Ast : in Mal.T) is
   begin
      Ada.Text_IO.Unbounded_IO.Put_Line (Printer.Pr_Str (Ast));
   end Print;

   function Read return Mal.T is (Reader.Read_Str (Readline.Input ("user> ")));

   procedure Rep (Env : in Envs.Map) is
   begin
      Print (Eval (Read, Env));
   end Rep;

   ----------------------------------------------------------------------

   function Addition    is new Generic_Mal_Operator ("+");
   function Subtraction is new Generic_Mal_Operator ("-");
   function Product     is new Generic_Mal_Operator ("*");
   function Division    is new Generic_Mal_Operator ("/");

   Repl : Envs.Map;
begin
   Repl.Insert ("+", Addition   'Unrestricted_Access);
   Repl.Insert ("-", Subtraction'Unrestricted_Access);
   Repl.Insert ("*", Product    'Unrestricted_Access);
   Repl.Insert ("/", Division   'Unrestricted_Access);
   loop
      begin
         Rep (Repl);
      exception
         when Readline.End_Of_File =>
            exit;
         when Reader.Empty_Source =>
            null;
         when E : Reader.Reader_Error | Unknown_Key =>
            Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
         --  Other exceptions are unexpected.
      end;
   end loop;
   Ada.Text_IO.New_Line;
end Step2_Eval;
