with Ada.Characters.Latin_1;
with Atoms;
with Lists;
with Maps;
with Strings;

package body Printer is

   use Ada.Strings.Unbounded;
   use Types;

   procedure Print_Form (Buffer         : in out Unbounded_String;
                         Ast            : in     Mal_Type;
                         Print_Readably : in     Boolean);
   procedure Print_List (Buffer         : in out Unbounded_String;
                         List           : in     Lists.Ptr;
                         Print_Readably : in     Boolean)
     with Inline;
   procedure Print_Function (Buffer         : in out Unbounded_String;
                             Formals        : in     Lists.Ptr;
                             Expression     : in     Atoms.Ptr;
                             Print_Readably : in     Boolean)
     with Inline;
   procedure Print_Map (Buffer         : in out Unbounded_String;
                        Map            : in     Maps.Ptr;
                        Print_Readably : in     Boolean)
     with Inline;

   ----------------------------------------------------------------------

   procedure Print_Form (Buffer         : in out Unbounded_String;
                         Ast            : in     Mal_Type;
                         Print_Readably : in     Boolean) is
   begin
      case Ast.Kind is

         when Kind_Nil =>
            Append (Buffer, "nil");

         when Kind_Boolean =>
            if Ast.Boolean_Value then
               Append (Buffer, "true");
            else
               Append (Buffer, "false");
            end if;

         when Kind_Symbol =>
            Append (Buffer, Ast.S.Deref);

         when Kind_Number =>
            declare
               Img : constant String := Integer'Image (Ast.Integer_Value);
               F   : Positive := Img'First;
            begin
               if Img (F) = ' ' then
                  F := F + 1;
               end if;
               Append (Buffer, Img (F .. Img'Last));
            end;

         when Kind_Keyword =>
            Append (Buffer, ':');
            Append (Buffer, Ast.S.Deref);

         when Kind_String =>
            if Print_Readably then
               Append (Buffer, '"');
               for C of Ast.S.Deref loop
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
               Append (Buffer, '"');
            else
               Append (Buffer, Ast.S.Deref);
            end if;

         when Kind_List =>
            Append (Buffer, '(');
            Print_List (Buffer, Ast.L, Print_Readably);
            Append (Buffer, ')');
         when Kind_Vector =>
            Append (Buffer, '[');
            Print_List (Buffer, Ast.L, Print_Readably);
            Append (Buffer, ']');

         when Kind_Map =>
            Print_Map (Buffer, Ast.Map, Print_Readably);

         when Kind_Native =>
            Append (Buffer, "#<built-in>");
         when Kind_Function =>
            Append (Buffer, "#<function ");
            Print_Function (Buffer, Ast.Formals, Ast.Expression,
                            Print_Readably);
            Append (Buffer, '>');
         when Kind_Macro =>
            Append (Buffer, "#<macro ");
            Print_Function (Buffer, Ast.Mac_Formals, Ast.Mac_Expression,
                            Print_Readably);
            Append (Buffer, '>');

         when Kind_Atom =>
            Append (Buffer, "(atom ");
            Print_Form (Buffer, Ast.Reference.Deref, Print_Readably);
            Append (Buffer, ')');

      end case;
   end Print_Form;

   procedure Print_Function (Buffer         : in out Unbounded_String;
                             Formals        : in     Lists.Ptr;
                             Expression     : in     Atoms.Ptr;
                             Print_Readably : in     Boolean) is
   begin
      if 0 < Formals.Length then
         Print_List (Buffer, Formals, Print_Readably);
         Append (Buffer, " -> ");
         Print_Form (Buffer, Expression.Deref, Print_Readably);
      end if;
   end Print_Function;

   procedure Print_List (Buffer         : in out Unbounded_String;
                         List           : in     Lists.Ptr;
                         Print_Readably : in     Boolean) is
   begin
      if 1 <= List.Length then
         Print_Form (Buffer, List.Element (1), Print_Readably);
         for I in 2 .. List.Length loop
            Append (Buffer, ' ');
            Print_Form (Buffer, List.Element (I), Print_Readably);
         end loop;
      end if;
   end Print_List;

   procedure Print_Map (Buffer         : in out Unbounded_String;
                        Map            : in     Maps.Ptr;
                        Print_Readably : in     Boolean)
   is
      Is_First : Boolean := True;
      procedure Process (Key     : in Mal_Type;
                         Element : in Mal_Type);
      procedure Process (Key     : in Mal_Type;
                         Element : in Mal_Type) is
      begin
         if Is_First then
            Is_First := False;
         else
            Append (Buffer, ' ');
         end if;
         Print_Form (Buffer, Key, Print_Readably);
         Append (Buffer, ' ');
         Print_Form (Buffer, Element, Print_Readably);
      end Process;
   begin
      Append (Buffer, '{');
      Map.Iterate (Process'Access);
      Append (Buffer, '}');
   end Print_Map;

   function Pr_Str (Ast            : in Mal_Type;
                    Print_Readably : in Boolean  := True)
                   return Unbounded_String
   is
      Result : Unbounded_String;
   begin
      Print_Form (Result, Ast, Print_Readably);
      return Result;
   end Pr_Str;

end Printer;
