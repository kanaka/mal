with Ada.Characters.Latin_1;

with Types.Atoms;
with Types.Fns;
with Types.Sequences;
with Types.Symbols;
with Types.Maps;

package body Printer is

   use Ada.Strings.Unbounded;
   use Types;

   procedure Pr_Str (Buffer   : in out Unbounded_String;
                     Ast      : in     Mal.T;
                     Readably : in     Boolean          := True)
   is

      procedure Print_Form (Form_Ast : in Mal.T);
      --  The recursive function traversing Ast for Pr_Str.
      --  Form_Ast is the current node.

      --  Helpers for Print_Form.
      procedure Print_Number   (Number : in Integer)              with Inline;
      procedure Print_List     (List   : in Sequences.Instance)   with Inline;
      procedure Print_Map      (Map    : in Maps.Instance)        with Inline;
      procedure Print_Readably (S      : in Unbounded_String)     with Inline;
      procedure Print_Function (Fn     : in Fns.Instance)         with Inline;

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
               Append (Buffer, Symbols.To_String (Form_Ast.Symbol));
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
               Print_List (Form_Ast.Sequence.all);
               Append (Buffer, ')');
            when Kind_Vector =>
               Append (Buffer, '[');
               Print_List (Form_Ast.Sequence.all);
               Append (Buffer, ']');
            when Kind_Map =>
               Append (Buffer, '{');
               Print_Map (Form_Ast.Map.all);
               Append (Buffer, '}');
            when Kind_Builtin | Kind_Builtin_With_Meta =>
               Append (Buffer, "#<built-in>");
            when Kind_Fn =>
               Append (Buffer, "#<function (");
               Print_Function (Form_Ast.Fn.all);
               Append (Buffer, '>');
            when Kind_Macro =>
               Append (Buffer, "#<macro (");
               Print_Function (Form_Ast.Fn.all);
               Append (Buffer, '>');
            when Kind_Atom =>
               Append (Buffer, "(atom ");
               Print_Form (Form_Ast.Atom.all.Deref);
               Append (Buffer, ')');
         end case;
      end Print_Form;

      procedure Print_Function (Fn : in Fns.Instance) is
         Started : Boolean := False;
      begin
         Append (Buffer, '(');
         for Param of Fn.Params loop
            if Started then
               Append (Buffer, ' ');
            else
               Started := True;
            end if;
            Append (Buffer, Symbols.To_String (Param));
         end loop;
         Append (Buffer, ") -> ");
         Print_Form (Fn.Ast);
      end Print_Function;

      procedure Print_List (List : in Sequences.Instance) is
      begin
         if 0 < List.Length then
            Print_Form (List (1));
            for I in 2 .. List.Length loop
               Append (Buffer, ' ');
               Print_Form (List (I));
            end loop;
         end if;
      end Print_List;

      procedure Print_Map (Map : in Maps.Instance) is
         procedure Process (Key     : in Mal.T;
                            Element : in Mal.T) with Inline;
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

      ----------------------------------------------------------------------

   begin                                --  Pr_Str
      Print_Form (Ast);
   end Pr_Str;

   function Pr_Str (Ast      : in Mal.T;
                    Readably : in Boolean := True) return Unbounded_String
   is
   begin
      return Buffer : Unbounded_String do
         Pr_Str (Buffer, Ast, Readably);
      end return;
   end Pr_Str;

end Printer;
