with Ada.Characters.Latin_1;

with Types.Atoms;
with Types.Functions;
with Types.Lists;
with Types.Maps;

package body Printer is

   use Ada.Strings.Unbounded;
   use Types;

   procedure Print_Form (Buffer   : in out Unbounded_String;
                         Ast      : in     Mal.T;
                         Readably : in     Boolean);
   procedure Print_List (Buffer   : in out Unbounded_String;
                         List     : in     Lists.Ptr;
                         Readably : in     Boolean) with Inline;
   procedure Print_Function (Buffer   : in out Unbounded_String;
                             Fn       : in     Functions.Ptr;
                             Readably : in     Boolean) with Inline;
   procedure Print_Map (Buffer   : in out Unbounded_String;
                        Map      : in     Maps.Ptr;
                        Readably : in     Boolean) with Inline;

   ----------------------------------------------------------------------

   procedure Print_Form (Buffer   : in out Unbounded_String;
                         Ast      : in     Mal.T;
                         Readably : in     Boolean) is
   begin
      case Ast.Kind is
         when Kind_Nil =>
            Append (Buffer, "nil");
         when Kind_Boolean =>
            if Ast.Ada_Boolean then
               Append (Buffer, "true");
            else
               Append (Buffer, "false");
            end if;
         when Kind_Symbol =>
            Append (Buffer, Ast.Symbol.To_String);
         when Kind_Number =>
            declare
               Img : constant String := Ast.Ada_Number'Img;
               F   : Positive := Img'First;
            begin
               if Img (F) = ' ' then
                  F := F + 1;
               end if;
               Append (Buffer, Img (F .. Img'Last));
            end;
         when Kind_Keyword =>
            Append (Buffer, ':');
            Append (Buffer, Ast.S);
         when Kind_String =>
            if Readably then
               Append (Buffer, '"');
               declare
                  C : Character;
               begin
                  for I in 1 .. Length (Ast.S) loop
                     C := Element (Ast.S, I);
                     case C is
                        when '"' | '\' =>
                           Append (Buffer, '\');
                           Append (Buffer, C);
                        when Ada.Characters.Latin_1.LF =>
                           Append (Buffer, "\n");
                        when others =>
                           Append (Buffer, C);
                     end case;
                  end loop;
               end;
               Append (Buffer, '"');
            else
               Append (Buffer, Ast.S);
            end if;
         when Kind_List =>
            Append (Buffer, '(');
            Print_List (Buffer, Ast.L, Readably);
            Append (Buffer, ')');
         when Kind_Vector =>
            Append (Buffer, '[');
            Print_List (Buffer, Ast.L, Readably);
            Append (Buffer, ']');
         when Kind_Map =>
            Print_Map (Buffer, Ast.Map, Readably);
         when Kind_Builtin | Kind_Builtin_With_Meta =>
            Append (Buffer, "#<built-in>");
         when Kind_Function =>
            Append (Buffer, "#<function ");
            Print_Function (Buffer, Ast.Function_Value, Readably);
            Append (Buffer, '>');
         when Kind_Macro =>
            Append (Buffer, "#<macro ");
            Print_Function (Buffer, Ast.Function_Value, Readably);
            Append (Buffer, '>');
         when Kind_Atom =>
            Append (Buffer, "(atom ");
            Print_Form (Buffer, Atoms.Deref (Mal.T_Array'(1 => Ast)),
                        Readably);
            Append (Buffer, ')');
      end case;
   end Print_Form;

   procedure Print_Function (Buffer   : in out Unbounded_String;
                             Fn       : in     Functions.Ptr;
                             Readably : in     Boolean) is
   begin
      Print_List (Buffer, Fn.Formals, Readably);
      Append (Buffer, " -> ");
      Print_Form (Buffer, Fn.Expression, Readably);
   end Print_Function;

   procedure Print_List (Buffer   : in out Unbounded_String;
                         List     : in     Lists.Ptr;
                         Readably : in     Boolean) is
   begin
      if 0 < List.Length then
         Print_Form (Buffer, List.Element (1), Readably);
         for I in 2 .. List.Length loop
            Append (Buffer, ' ');
            Print_Form (Buffer, List.Element (I), Readably);
         end loop;
      end if;
   end Print_List;

   procedure Print_Map (Buffer   : in out Unbounded_String;
                        Map      : in     Maps.Ptr;
                        Readably : in     Boolean) is
      Is_First : Boolean := True;
      procedure Process (Key     : in Mal.T;
                         Element : in Mal.T);
      procedure Iterate is new Maps.Iterate (Process);
      procedure Process (Key     : in Mal.T;
                         Element : in Mal.T) is
      begin
         if Is_First then
            Is_First := False;
         else
            Append (Buffer, ' ');
         end if;
         Print_Form (Buffer, Key, Readably);
         Append (Buffer, ' ');
         Print_Form (Buffer, Element, Readably);
      end Process;
   begin
      Append (Buffer, '{');
      Iterate (Map);
      Append (Buffer, '}');
   end Print_Map;

   function Pr_Str (Ast      : in Mal.T;
                    Readably : in Boolean := True) return Unbounded_String is
   begin
      return Buffer : Unbounded_String do
         Print_Form (Buffer, Ast, Readably);
      end return;
   end Pr_Str;

end Printer;
