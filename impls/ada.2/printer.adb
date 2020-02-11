with Ada.Characters.Latin_1;

with Types.Atoms;
with Types.Fns;
with Types.Maps;
pragma Warnings (Off, "unit ""Types.Sequences"" is not referenced");
with Types.Sequences;
pragma Warnings (On, "unit ""Types.Sequences"" is not referenced");

package body Printer is

   use Ada.Strings.Unbounded;
   use all type Types.Kind_Type;

   procedure Pr_Str (Buffer   : in out Unbounded_String;
                     Ast      : in     Types.T;
                     Readably : in     Boolean          := True)
   is

      procedure Print_Form (Form_Ast : in Types.T);
      --  The recursive function traversing Ast for Pr_Str.
      --  Form_Ast is the current node.

      --  Helpers for Print_Form.
      procedure Print_Number   (Number : in Integer);
      procedure Print_List     (List   : in Types.T_Array);
      procedure Print_Map      (Map    : in Types.Maps.Instance);
      procedure Print_Readably (S      : in String);
      procedure Print_String   (S      : in String);

      ----------------------------------------------------------------------

      procedure Print_Form (Form_Ast : in Types.T) is
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
               Form_Ast.Str.all.Query_Element (Print_String'Access);
            when Kind_Number =>
               Print_Number (Form_Ast.Number);
            when Kind_Keyword =>
               Append (Buffer, ':');
               Form_Ast.Str.all.Query_Element (Print_String'Access);
            when Kind_String =>
               if Readably then
                  Append (Buffer, '"');
                  Form_Ast.Str.all.Query_Element (Print_Readably'Access);
                  Append (Buffer, '"');
               else
                  Form_Ast.Str.all.Query_Element (Print_String'Access);
               end if;
            when Kind_List =>
               Append (Buffer, '(');
               Print_List (Form_Ast.Sequence.all.Data);
               Append (Buffer, ')');
            when Kind_Vector =>
               Append (Buffer, '[');
               Print_List (Form_Ast.Sequence.all.Data);
               Append (Buffer, ']');
            when Kind_Map =>
               Append (Buffer, '{');
               Print_Map (Form_Ast.Map.all);
               Append (Buffer, '}');
            when Kind_Builtin | Kind_Builtin_With_Meta =>
               Append (Buffer, "#<built-in>");
            when Kind_Fn =>
               Append (Buffer, "#<function (");
               Print_List (Form_Ast.Fn.all.Params.all.Data);
               Append (Buffer, ") -> ");
               Print_Form (Form_Ast.Fn.all.Ast);
               Append (Buffer, '>');
            when Kind_Macro =>
               Append (Buffer, "#<macro (");
               Print_List (Form_Ast.Fn.all.Params.all.Data);
               Append (Buffer, ") -> ");
               Print_Form (Form_Ast.Fn.all.Ast);
               Append (Buffer, '>');
            when Kind_Atom =>
               Append (Buffer, "(atom ");
               Print_Form (Form_Ast.Atom.all.Deref);
               Append (Buffer, ')');
         end case;
      end Print_Form;

      procedure Print_List (List : in Types.T_Array) is
      begin
         if 0 < List'Length then
            Print_Form (List (List'First));
            for I in List'First + 1 .. List'Last loop
               Append (Buffer, ' ');
               Print_Form (List (I));
            end loop;
         end if;
      end Print_List;

      procedure Print_Map (Map : in Types.Maps.Instance) is
         use all type Types.Maps.Cursor;
         Position : Types.Maps.Cursor := Map.First;
      begin
         if Has_Element (Position) then
            loop
               Print_Form (Key (Position));
               Append (Buffer, ' ');
               Print_Form (Element (Position));
               Next (Position);
               exit when not Has_Element (Position);
               Append (Buffer, ' ');
            end loop;
         end if;
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

      procedure Print_Readably (S : in String) is
      begin
         for C of S loop
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
      end Print_Readably;

      procedure Print_String (S : in String) is
      begin
         Append (Buffer, S);
      end Print_String;

      ----------------------------------------------------------------------

   begin                                --  Pr_Str
      Print_Form (Ast);
   end Pr_Str;

   function Pr_Str (Ast      : in Types.T;
                    Readably : in Boolean := True) return Unbounded_String
   is
   begin
      return Buffer : Unbounded_String do
         Pr_Str (Buffer, Ast, Readably);
      end return;
   end Pr_Str;

end Printer;
