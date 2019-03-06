with Ada.Characters.Latin_1;

with Types.Atoms;
with Types.Functions;
with Types.Symbols;
with Types.Lists;
with Types.Maps;

package body Printer is

   use Ada.Strings.Unbounded;
   use Types;

   function Pr_Str (Ast      : in Mal.T;
                    Readably : in Boolean := True) return Unbounded_String
   is

      procedure Print_Form (Form_Ast : in Mal.T);
      --  The recursive function traversing Ast for Pr_Str.
      --  Form_Ast is the current node.

      --  Helpers for Print_Form.
      procedure Print_Number   (Number : in Integer)              with Inline;
      procedure Print_List     (List   : in Lists.Ptr)            with Inline;
      procedure Print_Map      (Map    : in Maps.Ptr)             with Inline;
      procedure Print_Readably (S      : in Unbounded_String)     with Inline;
      procedure Print_Symbols  (List   : in Symbols.Symbol_Array) with Inline;

      Buffer : Unbounded_String := Null_Unbounded_String;
      --  is appended the result character after character.

      ----------------------------------------------------------------------

      procedure Print_Form (Form_Ast : in Mal.T) is
      begin
         case Form_Ast.Kind is
            when Kind_Nil =>
               Append (Buffer, "nil");
            when Kind_Boolean =>
               if Form_Ast.Ada_Boolean then
                  Append (Buffer, "true");
               else
                  Append (Buffer, "false");
               end if;
            when Kind_Symbol =>
               Append (Buffer, Form_Ast.Symbol.To_String);
            when Kind_Number =>
               Print_Number (Form_Ast.Number);
            when Kind_Keyword =>
               Append (Buffer, ':');
               Append (Buffer, Form_Ast.S);
            when Kind_String =>
               if Readably then
                  Append (Buffer, '"');
                  Print_Readably (Form_Ast.S);
                  Append (Buffer, '"');
               else
                  Append (Buffer, Form_Ast.S);
               end if;
            when Kind_List =>
               Append (Buffer, '(');
               Print_List (Form_Ast.List);
               Append (Buffer, ')');
            when Kind_Vector =>
               Append (Buffer, '[');
               Print_List (Form_Ast.List);
               Append (Buffer, ']');
            when Kind_Map =>
               Append (Buffer, '{');
               Print_Map (Form_Ast.Map);
               Append (Buffer, '}');
            when Kind_Builtin | Kind_Builtin_With_Meta =>
               Append (Buffer, "#<built-in>");
            when Kind_Function =>
               Append (Buffer, "#<function (");
               Print_Symbols (Form_Ast.Fn.Params);
               Append (Buffer, ") -> ");
               Print_Form (Form_Ast.Fn.Ast);
               Append (Buffer, '>');
            when Kind_Macro =>
               Append (Buffer, "#<macro (");
               Print_Symbols (Form_Ast.Fn.Params);
               Append (Buffer, ") -> ");
               Print_Form (Form_Ast.Fn.Ast);
               Append (Buffer, '>');
            when Kind_Atom =>
               Append (Buffer, "(atom ");
               Print_Form (Atoms.Deref (Form_Ast.Atom));
               Append (Buffer, ')');
         end case;
      end Print_Form;

      procedure Print_List (List : in Lists.Ptr) is
         Started : Boolean := False;
      begin
         for I in 1 .. List.Length loop
            if Started then
               Append (Buffer, ' ');
            else
               Started := True;
            end if;
            Print_Form (List.Element (I));
         end loop;
      end Print_List;

      procedure Print_Map (Map : in Maps.Ptr) is
         procedure Process (Key     : in Mal.T;
                            Element : in Mal.T);
         procedure Iterate is new Maps.Iterate (Process);
         Started : Boolean := False;
         procedure Process (Key     : in Mal.T;
                            Element : in Mal.T)
         is
         begin
            if Started then
               Append (Buffer, ' ');
            else
               Started := True;
            end if;
            Print_Form (Key);
            Append (Buffer, ' ');
            Print_Form (Element);
         end Process;
      begin
         Iterate (Map);
      end Print_Map;

      procedure Print_Number (Number : in Integer) is
         Image : constant String := Integer'Image (Number);
         First : Positive := Image'First;
      begin
         if Image (First) = ' ' then
            First := First + 1;
         end if;
         Append (Buffer, Image (First .. Image'Last));
      end Print_Number;

      procedure Print_Readably (S : in Unbounded_String) is
      begin
         for I in 1 .. Length (S) loop
            declare
               C : constant Character := Element (S, I);
            begin
               case C is
                  when '"' | '\' =>
                     Append (Buffer, '\');
                     Append (Buffer, C);
                  when Ada.Characters.Latin_1.LF =>
                     Append (Buffer, "\n");
                  when others =>
                     Append (Buffer, C);
               end case;
            end;
         end loop;
      end Print_Readably;

      procedure Print_Symbols (List : in Symbols.Symbol_Array) is
         Started : Boolean := False;
      begin
         for S of List loop
            if Started then
               Append (Buffer, ' ');
            else
               Started := True;
            end if;
            Append (Buffer, S.To_String);
         end loop;
      end Print_Symbols;

      ----------------------------------------------------------------------

   begin                                --  Pr_Str
      Print_Form (Form_Ast => Ast);
      return Buffer;
   end Pr_Str;

end Printer;
