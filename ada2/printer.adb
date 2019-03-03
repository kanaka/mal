with Ada.Characters.Latin_1;

with Types.Atoms;
with Types.Functions;
with Types.Lists;
with Types.Maps;

package body Printer is

   function Pr_Str (Ast      : in Types.Mal.T;
                    Readably : in Boolean     := True)
                   return Ada.Strings.Unbounded.Unbounded_String
   is

      use Ada.Strings.Unbounded;
      use Types;

      Buffer : Unbounded_String := Null_Unbounded_String;
      --  is appended the result character after character.

      procedure Print_Form (Form_Ast : in Mal.T);
      --  The recursive function traversing Ast for Pr_Str.
      --  Form_Ast is the current node.

      ----------------------------------------------------------------------

      procedure Print_Form (Form_Ast : in Mal.T) is

         procedure Print_List (List : in Lists.Ptr) with Inline;
         --  An helper for Print_Form.

         procedure Print_List (List : in Lists.Ptr) is
         begin
            if 0 < List.Length then
               Print_Form (List.Element (1));
               for I in 2 .. List.Length loop
                  Append (Buffer, ' ');
                  Print_Form (List.Element (I));
               end loop;
            end if;
         end Print_List;

      begin                             --  Print_Form
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
               declare
                  Img : constant String := Integer'Image (Form_Ast.Ada_Number);
                  F   : Positive := Img'First;
               begin
                  if Img (F) = ' ' then
                     F := F + 1;
                  end if;
                  Append (Buffer, Img (F .. Img'Last));
               end;
            when Kind_Keyword =>
               Append (Buffer, ':');
               Append (Buffer, Form_Ast.S);
            when Kind_String =>
               if Readably then
                  declare
                     C : Character;
                  begin
                     Append (Buffer, '"');
                     for I in 1 .. Length (Form_Ast.S) loop
                        C := Element (Form_Ast.S, I);
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
                  end;
               else
                  Append (Buffer, Form_Ast.S);
               end if;
            when Kind_List =>
               Append (Buffer, '(');
               Print_List (Form_Ast.L);
               Append (Buffer, ')');
            when Kind_Vector =>
               Append (Buffer, '[');
               Print_List (Form_Ast.L);
               Append (Buffer, ']');
            when Kind_Map =>
               Append (Buffer, '{');
               declare
                  Is_First : Boolean := True;
                  procedure Process (Key     : in Mal.T;
                                     Element : in Mal.T);
                  procedure Iterate is new Maps.Iterate (Process);
                  procedure Process (Key     : in Mal.T;
                                     Element : in Mal.T)
                  is
                  begin
                     if Is_First then
                        Is_First := False;
                     else
                        Append (Buffer, ' ');
                     end if;
                     Print_Form (Key);
                     Append (Buffer, ' ');
                     Print_Form (Element);
                  end Process;
               begin
                  Iterate (Form_Ast.Map);
               end;
               Append (Buffer, '}');
            when Kind_Builtin | Kind_Builtin_With_Meta =>
               Append (Buffer, "#<built-in>");
            when Kind_Function =>
               Append (Buffer, "#<function ");
               Print_List (Form_Ast.Function_Value.Formals);
               Append (Buffer, " -> ");
               Print_Form (Form_Ast.Function_Value.Expression);
               Append (Buffer, '>');
            when Kind_Macro =>
               Append (Buffer, "#<macro ");
               Print_List (Form_Ast.Function_Value.Formals);
               Append (Buffer, " -> ");
               Print_Form (Form_Ast.Function_Value.Expression);
               Append (Buffer, '>');
            when Kind_Atom =>
               Append (Buffer, "(atom ");
               Print_Form (Atoms.Deref (Mal.T_Array'(1 => Form_Ast)));
               Append (Buffer, ')');
         end case;
      end Print_Form;

      ----------------------------------------------------------------------

   begin                                --  Pr_Str
      Print_Form (Form_Ast => Ast);
      return Buffer;
   end Pr_Str;

end Printer;
