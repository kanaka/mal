with Ada.Characters.Latin_1;
with Ada.Text_IO;

package body Types is

   package ACL renames Ada.Characters.Latin_1;

   function To_String (T : Mal_Type) return String is
      use Ada.Strings.Unbounded;
   begin
      case T.Sym_Type is
         when Int =>
            declare
               Res : String := Integer'Image (T.Int_Val);
            begin
               if Res (1) = ' ' then
                 return Res (2..Res'Last);
               else
                 return "int> " & Res;
               end if;
            end;
         when List =>

            declare
               UBS : Unbounded_String := Null_Unbounded_String;
               C : Lists.Cursor;
               use type Lists.Cursor;
               First_Pass : Boolean := True;
            begin
               if Lists.Is_Empty (T.The_List) then
                  return "()";
               end if;
               C := Lists.First (T.The_List);
               loop
                  if First_Pass then
                     First_Pass := False;
                  else
                     Append (UBS, " ");
                  end if;
                  UBStrings.Append (UBS, To_String (Lists.Element (C).all));
               exit when C = Lists.Last (T.The_List);
                  C := Lists.Next (C);
               end loop;
               return "(" & To_String (UBS) & ")";
            end;
         when Sym =>
            return "" & T.Symbol;
         when Str =>
            -- The_String includes the quotation marks.
            return To_String (T.The_String);
         when Atom =>
            return To_String (T.The_Atom);
      end case;
   end To_String;

end Types;
