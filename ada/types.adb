with Ada.Characters.Latin_1;
with Ada.Text_IO;
with Ada.Unchecked_Deallocation;

package body Types is

   package ACL renames Ada.Characters.Latin_1;


   function Opening (LT : List_Types) return Character is
      Res : Character;
   begin
      case LT is
         when List_List =>
            Res := '(';
         when Vector_List =>
            Res := '[';
         when Hashed_List =>
            Res := '{';
      end case;
      return Res;
   end Opening;


   function Closing (LT : List_Types) return Character is
      Res : Character;
   begin
      case LT is
         when List_List =>
            Res := ')';
         when Vector_List =>
            Res := ']';
         when Hashed_List =>
            Res := '}';
      end case;
      return Res;
   end Closing;


   function Mal_Type_To_String (T : Mal_Type) return String is
      use Ada.Strings.Unbounded;
   begin
      case T.Sym_Type is
         when Int =>
            declare
               Res : String := Mal_Integer'Image (T.Int_Val);
            begin
               if Res (1) = ' ' then
                 return Res (2..Res'Last);
               else
                 return Res;
               end if;
            end;
         when Floating =>
            declare
               Res : String := Mal_Float'Image (T.Float_Val);
            begin
               if Res (1) = ' ' then
                 return Res (2..Res'Last);
               else
                 return Res;
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
                  return Opening (T.List_Type) & Closing (T.List_Type);
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
               return Opening (T.List_Type) &
                      To_String (UBS) &
                      Closing (T.List_Type);
            end;
         when Sym =>
            return "" & T.Symbol;
         when Str =>
            -- The_String includes the quotation marks.
            return To_String (T.The_String);
         when Atom =>
            return To_String (T.The_Atom);
         when Unitary =>
            case T.The_Function is
               when Quote =>
                  return "(quote " & To_String (T.The_Operand.all) & ")";
               when Unquote =>
                  return "(unquote " & To_String (T.The_Operand.all) & ")";
               when Quasiquote =>
                  return "(quasiquote " & To_String (T.The_Operand.all) & ")";
               when Splice_Unquote =>
                  return
                    "(splice-unquote " & To_String (T.The_Operand.all) & ")";
               when Deref =>
                  return
                    "(deref " & To_String (T.The_Operand.all) & ")";
            end case;
         when Error =>
            return To_String (T.Error_Msg);
      end case;
   end Mal_Type_To_String;


   function To_String (T : Mal_Type) return String is
   begin
      if T.Meta /= null then
         return "(with-meta " &
                Mal_Type_To_String (T) & " " &
                Mal_Type_To_String (T.Meta.all) & ")";
      else
         return Mal_Type_To_String (T);
      end if;
   end To_String;


   procedure Free is
     new Ada.Unchecked_Deallocation (Mal_Type, Mal_Type_Access);

   procedure Delete_Tree (MTA : in out Mal_Type_Access) is
   begin
      if MTA /= null then
         case MTA.Sym_Type is
            when List =>
               declare
                  C, D : Lists.Cursor;
                  Tmp : Mal_Type_Access;
               begin
                  C := MTA.The_List.First;
                  while Lists.Has_Element (C) loop
                     -- Delete item at this position in list.
                     Tmp := Lists.Element (C);
                     Delete_Tree (Tmp); -- must be variable (out param)
                     -- Access and save the next cursor pos.
                     D := Lists.Next (C);
                     -- Delete this item position from list.
                     Lists.Delete (MTA.The_List, C);
                     -- Restore the next cursor position.
                     C := D;
                  end loop;
               end;
            when Unitary =>
               Delete_Tree (MTA.The_Operand);
            when others => Free (MTA);
         end case;
      end if;
   end Delete_Tree;


end Types;
