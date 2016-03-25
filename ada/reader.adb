with Ada.IO_Exceptions;
with Ada.Characters.Latin_1;
with Ada.Exceptions;
with Ada.Strings.Maps.Constants;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Smart_Pointers;
with Types.Vector;
with Types.Hash_Map;

package body Reader is

   use Types;

   package ACL renames Ada.Characters.Latin_1;

   type Lexemes is (Ignored_Tok,
                    Start_List_Tok, Start_Vector_Tok, Start_Hash_Tok,
                    Meta_Tok, Deref_Tok,
                    Quote_Tok, Quasi_Quote_Tok, Splice_Unq_Tok, Unquote_Tok,
                    Int_Tok, Float_Tok,
                    Str_Tok, Sym_Tok);

   type Token (ID : Lexemes := Ignored_Tok) is record
      case ID is
         when Int_Tok =>
            Int_Val : Mal_Integer;
         when Float_Tok =>
            Float_Val : Mal_Float;
         when Str_Tok | Sym_Tok =>
            Start_Char, Stop_Char : Natural;
         when others => null;
      end case;
   end record;

   Lisp_Whitespace : constant Ada.Strings.Maps.Character_Set :=
     Ada.Strings.Maps.To_Set
       (ACL.HT & ACL.LF & ACL.CR & ACL.Space & ACL.Comma);

   -- [^\s\[\]{}('"`,;)]
   Terminator_Syms : Ada.Strings.Maps.Character_Set :=
     Ada.Strings.Maps."or"
       (Lisp_Whitespace, 
        Ada.Strings.Maps.To_Set ("[]{}('""`,;)"));

   -- The unterminated string error
   String_Error : exception;


   function Convert_String (S : String) return String is
      use Ada.Strings.Unbounded;
      Res : Unbounded_String;
      I : Positive;
      Str_Last : Natural;
   begin
      Str_Last := S'Last;
      I := S'First;
      while I <= Str_Last loop
         if S (I) = '\' then
            if I+1 > Str_Last then
               Append (Res, S (I));
               I := I + 1;
            elsif S (I+1) = 'n' then
               Append (Res, Ada.Characters.Latin_1.LF);
               I := I + 2;
            elsif S (I+1) = '"' then
               Append (Res, S (I+1));
               I := I + 2;
            elsif S (I+1) = '\' then
               Append (Res, S (I+1));
               I := I + 2;
            else
               Append (Res, S (I));
               I := I + 1;
            end if;
         else
            Append (Res, S (I));
            I := I + 1;
         end if;
      end loop;
      return To_String (Res);
   end Convert_String;

   Str_Len : Natural := 0;
   Saved_Line : Ada.Strings.Unbounded.Unbounded_String;
   Char_To_Read : Natural := 1;

   function Get_Token return Token is
      Res : Token;
      I, J : Natural;
      use Ada.Strings.Unbounded;
   begin

      <<Tail_Call_Opt>>

      -- Skip over whitespace...
      I := Char_To_Read;
      while I <= Str_Len and then
            Ada.Strings.Maps.Is_In (Element (Saved_Line, I), Lisp_Whitespace) loop
         I := I + 1;
      end loop;

      -- Filter out lines consisting of only whitespace
      if I > Str_Len then
         return (ID => Ignored_Tok);
      end if;

      J := I;

      case Element (Saved_Line, J) is

         when ''' => Res := (ID => Quote_Tok); Char_To_Read := J+1;

         when '`' => Res := (ID => Quasi_Quote_Tok); Char_To_Read := J+1;

         when '~' => -- Tilde

            if J+1 <= Str_Len and then Element (Saved_Line, J+1) = '@' then
               Res := (ID => Splice_Unq_Tok);
               Char_To_Read := J+2;
            else
               -- Just a Tilde
               Res := (ID => Unquote_Tok);
               Char_To_Read := J+1;
            end if;

         when '(' => Res := (ID => Start_List_Tok); Char_To_Read := J+1;
         when '[' => Res := (ID => Start_Vector_Tok); Char_To_Read := J+1;
         when '{' => Res := (ID => Start_Hash_Tok); Char_To_Read := J+1;

         when '^' => Res := (ID => Meta_Tok); Char_To_Read := J+1;
         when '@' => Res := (ID => Deref_Tok); Char_To_Read := J+1;

         when ']' | '}' | ')' =>
            
            Res := (ID => Sym_Tok, Start_Char => J, Stop_Char => J);
            Char_To_Read := J+1;

         when '"' => -- a string

            -- Skip over "
            J := J + 1;
            while J <= Str_Len and then
               (Element (Saved_Line, J) /= '"' or else
                 Element (Saved_Line, J-1) = '\') loop
               J := J + 1;
            end loop;

            -- So we either ran out of string..
            if J > Str_Len then
               raise String_Error;
            end if;

            -- or we reached an unescaped "
            Res := (ID => Str_Tok, Start_Char => I, Stop_Char => J);
            Char_To_Read := J + 1;

         when ';' => -- a comment

            -- Read to the end of the line or until
            -- the saved_line string is exhausted.
            -- NB if we reach the end we don't care
            -- what the last char was.
            while J < Str_Len and Element (Saved_Line, J) /= ACL.LF loop
               J := J + 1;
            end loop;
            if J = Str_Len then
               Res := (ID => Ignored_Tok);
            else
               Char_To_Read := J + 1;
               -- was: Res := Get_Token;
               goto Tail_Call_Opt;
            end if;

         when others => -- an atom

            while J <= Str_Len and then
               not Ada.Strings.Maps.Is_In (Element (Saved_Line, J), Terminator_Syms) loop
               J := J + 1;
            end loop;

            -- Either we ran out of string or
            -- the one at J was the start of a new token
            Char_To_Read := J;
            J := J - 1;

            declare
               Dots : Natural;
               All_Digits : Boolean;
            begin
               -- check if all digits or .
               Dots := 0;
               All_Digits := True;
               for K in I .. J loop
                  if (K = I and K /= J) and then Element (Saved_Line, K) = '-' then
                     null;
                  elsif Element (Saved_Line, K) = '.' then
                     Dots := Dots + 1; 
                  elsif not (Element (Saved_Line, K) in '0' .. '9') then
                     All_Digits := False;
                     exit;
                  end if;
               end loop;

               if All_Digits then
                  if Dots = 0 then
                     Res :=
                       (ID => Int_Tok,
                        Int_Val => Mal_Integer'Value (Slice (Saved_Line, I, J)));
                  elsif Dots = 1 then
                     Res :=
                       (ID => Float_Tok,
                        Float_Val => Mal_Float'Value (Slice (Saved_Line, I, J)));
                  else
                     Res := (ID => Sym_Tok, Start_Char => I, Stop_Char => J);
                  end if;
               else
                  Res := (ID => Sym_Tok, Start_Char => I, Stop_Char => J);
               end if;

            end;

      end case;

      return Res;

   end Get_Token;


   function Read_List (LT : Types.List_Types)
   return Types.Mal_Handle is

      MTA : Mal_Handle;

   begin

      MTA := Read_Form;

      declare
         List_SP : Mal_Handle;
         List_P : List_Class_Ptr;
         Close : String (1..1) := (1 => Types.Closing (LT));
      begin

         case LT is
            when List_List   => List_SP := New_List_Mal_Type (List_Type => LT);
            when Vector_List => List_SP := Vector.New_Vector_Mal_Type;
            when Hashed_List => List_SP := Hash_Map.New_Hash_Map_Mal_Type;
         end case;

         -- Need to append to a variable so...
         List_P := Deref_List_Class (List_SP);

         loop

            if Is_Null (MTA) then
               return New_Error_Mal_Type (Str => "expected '" & Close & "'");
            end if;

            exit when Deref (MTA).Sym_Type = Sym and then
                      Symbol_Mal_Type (Deref (MTA).all).Get_Sym = Close;

            Append (List_P.all, MTA);

            MTA := Read_Form;

         end loop;

         return List_SP;

      end;

   end Read_List;


   function Read_Form return Types.Mal_Handle is
      Tok : Token;
      MTS : Mal_Handle;
      use Ada.Strings.Unbounded;
   begin

      Tok := Get_Token;

      case Tok.ID is

         when Ignored_Tok => return Smart_Pointers.Null_Smart_Pointer;

         when Int_Tok => return New_Int_Mal_Type (Tok.Int_Val);

         when Float_Tok => return New_Float_Mal_Type (Tok.Float_Val);

         when Start_List_Tok => return Read_List (List_List);

         when Start_Vector_Tok => return Read_List (Vector_List);

         when Start_Hash_Tok => return Read_List (Hashed_List);

         when Meta_Tok =>
            
            declare
               Meta, Obj : Mal_Handle;
            begin
               Meta := Read_Form;
               Obj := Read_Form;
               return Make_New_List
                        ((1 => New_Symbol_Mal_Type ("with-meta"),
                          2 => Obj,
                          3 => Meta));
            end;

         when Deref_Tok =>

            return Make_New_List
                     ((1 => New_Symbol_Mal_Type ("deref"),
                       2 => Read_Form));

         when Quote_Tok =>

            return Make_New_List
                     ((1 => New_Symbol_Mal_Type ("quote"),
                       2 => Read_Form));

         when Quasi_Quote_Tok =>

            return Make_New_List
                     ((1 => New_Symbol_Mal_Type ("quasiquote"),
                       2 => Read_Form));

         when Splice_Unq_Tok =>

            return Make_New_List
                     ((1 =>  New_Symbol_Mal_Type ("splice-unquote"),
                       2 => Read_Form));

         when Unquote_Tok =>

            return Make_New_List
                     ((1 => New_Symbol_Mal_Type ("unquote"),
                       2 => Read_Form));

         when Str_Tok =>

            -- +/-1 strips out the double quotes.
            -- Convert_String converts backquoted charaters to raw format.
            return New_String_Mal_Type
                     (Convert_String
                       (Slice (Saved_Line, Tok.Start_Char + 1, Tok.Stop_Char - 1)));

         when Sym_Tok =>

            -- Mal interpreter is required to know about true, false and nil.
            declare
               S : String := Slice (Saved_Line, Tok.Start_Char, Tok.Stop_Char);
            begin
               if S = "true" then
                  return New_Bool_Mal_Type (True);
               elsif S = "false" then
                  return New_Bool_Mal_Type (False);
               elsif S = "nil" then
                  return New_Nil_Mal_Type;
               else
                  return New_Symbol_Mal_Type (S);
               end if;
            end;

      end case;

   end Read_Form;


   procedure Lex_Init (S : String) is
   begin
      Str_Len := S'Length;
      Saved_Line := Ada.Strings.Unbounded.To_Unbounded_String (S);
      Char_To_Read := 1;
   end Lex_Init;


   function Read_Str (S : String) return Types.Mal_Handle is
      I, Str_Len : Natural := S'Length;
   begin

      Lex_Init (S);

      return Read_Form;

   exception
      when String_Error =>
        return New_Error_Mal_Type (Str => "expected '""'");
   end Read_Str;
   

end Reader;
