with Ada.Characters.Latin_1;
with Ada.Text_IO;
with Ada.Unchecked_Deallocation;

package body Types is

   package ACL renames Ada.Characters.Latin_1;


   -- Smart Pointers section.

   overriding procedure Adjust (Object : in out Smart_Pointer) is
   begin
      if Object.Pointer /= null then
         Object.Pointer.Ref_Count := Object.Pointer.Ref_Count + 1;
      end if;
   end Adjust;

   procedure Free is
     new Ada.Unchecked_Deallocation (Mal_Type, Mal_Type_Accessor);

   overriding procedure Finalize (Object : in out Smart_Pointer) is
   begin
      if Object.Pointer /= null then
         Object.Pointer.Ref_Count := Object.Pointer.Ref_Count - 1;
         if Object.Pointer.Ref_Count = 0 then
            Free (Object.Pointer);
         end if;
      end if;
   end Finalize;

   function New_Ptr (Mal_Type : Mal_Type_Accessor) return Smart_Pointer is
   begin
      return Smart_Pointer'
         (Ada.Finalization.Controlled with Pointer => Mal_Type);
   end New_Ptr;

   function Deref (Ptr : Smart_Pointer) return Mal_Type_Accessor is
   begin
      return Ptr.Pointer;
   end Deref;


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
                  UBStrings.Append (UBS, To_String (Deref (Lists.Element (C)).all));
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
                  return "(quote " & To_String (Deref (T.The_Operand).all) & ")";
               when Unquote =>
                  return "(unquote " & To_String (Deref (T.The_Operand).all) & ")";
               when Quasiquote =>
                  return "(quasiquote " & To_String (Deref (T.The_Operand).all) & ")";
               when Splice_Unquote =>
                  return
                    "(splice-unquote " & To_String (Deref (T.The_Operand).all) & ")";
               when Deref =>
                  return
                    "(deref " & To_String (Deref (T.The_Operand).all) & ")";
            end case;
         when Error =>
            return To_String (T.Error_Msg);
      end case;
   end Mal_Type_To_String;


   function To_String (T : Mal_Type) return String is
   begin
      if T.Meta /= Null_Smart_Pointer then
         return "(with-meta " &
                Mal_Type_To_String (T) & " " &
                Mal_Type_To_String (Deref (T.Meta).all) & ")";
      else
         return Mal_Type_To_String (T);
      end if;
   end To_String;


end Types;
