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

   type Lexemes is (Whitespace, Comment,
                    Int_Tok, Float_Tok, Sym_Tok,
                    Nil_Tok, True_Tok, False_Tok,
                    LE_Tok, GE_Tok, Exp_Tok, Splice_Unq_Tok,
                    Str_Tok, Atom_Tok);

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

   subtype String_Indices is Integer range 0 .. Max_Line_Len;

   Str_Len : String_Indices := 0;
   Saved_Line : String (1..Max_Line_Len);
   Char_To_Read : String_Indices := 1;

   function Get_Token return Types.Mal_Handle is
      Res : Types.Mal_Handle;
      I, J : String_Indices;
      Dots : Natural;
      All_Digits : Boolean;
   begin

      <<Tail_Call_Opt>>
      I := Char_To_Read;
      while I <= Str_Len and then
            Ada.Strings.Maps.Is_In (Saved_Line (I), Lisp_Whitespace) loop
         I := I + 1;
      end loop;

      -- Filter out lines consisting of only whitespace
      if I > Str_Len then
         return Smart_Pointers.Null_Smart_Pointer;
      end if;

      J := I;
      case Saved_Line (J) is
         when '~' => -- Circumflex
            if J+1 <= Str_Len and then Saved_Line(J+1) = '@' then
               Res := New_Unitary_Mal_Type
                 (Func => Splice_Unquote,
                  Op => Smart_Pointers.Null_Smart_Pointer);
               Char_To_Read := J+2;
            else
               -- Just a circumflex
               Res := New_Atom_Mal_Type (Saved_Line (J..J));
               Char_To_Read := J+1;
            end if;
         when '[' | ']' |
              '{' | '}' |
              '(' | ')' |
              ''' | '`' |
              '^' | '@' =>
            
            Res := New_Atom_Mal_Type (Saved_Line (J..J));
            Char_To_Read := J+1;

         when '"' => -- a string

            -- Skip over "
            J := J + 1;
            while J <= Str_Len and then
               (Saved_Line (J) /= '"' or else
                 Saved_Line (J-1) = '\') loop
               J := J + 1;
            end loop;

            -- So we either ran out of string..
            if J > Str_Len then
               raise String_Error;
            end if;

            -- or we reached an unescaped "
            Res := New_String_Mal_Type
              (Str => Convert_String (Saved_Line (I .. J)));
            Char_To_Read := J + 1;

         when ';' => -- a comment

            -- Read to the end of the line or until
            -- the saved_line string is exhausted.
            -- NB if we reach the end we don't care
            -- what the last char was.
            while J < Str_Len and Saved_Line (J) /= ACL.LF loop
               J := J + 1;
            end loop;
            if J = Str_Len then
               Res := Smart_Pointers.Null_Smart_Pointer;
            else
               Char_To_Read := J + 1;
               -- was: Res := Get_Token;
               goto Tail_Call_Opt;
            end if;

         when others => -- an atom

            while J <= Str_Len and then
               not Ada.Strings.Maps.Is_In (Saved_Line (J), Terminator_Syms) loop
               J := J + 1;
            end loop;

            -- Either we ran out of string or
            -- the one at J was the start of a new token
            Char_To_Read := J;
            J := J - 1;

            -- check if all digits or .
            Dots := 0;
            All_Digits := True;
            for K in I .. J loop
               if Saved_Line (K) = '.' then
                  Dots := Dots + 1; 
               elsif not (Saved_Line (K) in '0' .. '9') then
                  All_Digits := False;
                  exit;
               end if;
            end loop;

            if All_Digits then
               if Dots = 0 then
                  Res := New_Int_Mal_Type
                    (Int => Mal_Integer'Value (Saved_Line (I .. J)));
               elsif Dots = 1 then
                  Res := New_Float_Mal_Type
                    (Floating => Mal_Float'Value (Saved_Line (I..J)));
               else
                  Res := New_Atom_Mal_Type (Saved_Line (I..J));
               end if;
            else
               Res := New_Atom_Mal_Type (Saved_Line (I..J));
            end if;

      end case;

      return Res;

   end Get_Token;


   -- Parsing
   function Read_Form return Types.Mal_Handle;

   function Read_List (LT : Types.List_Types)
   return Types.Mal_Handle is

      MTA : Mal_Handle;

   begin

      MTA := Read_Form;

      if Deref (MTA).Sym_Type = Atom and then
         Deref_Atom (MTA).Get_Atom = "fn*" then

         declare
            Params, Expr, Close_Lambda : Mal_Handle;
         begin
            Params := Read_Form;
            Expr := Read_Form;
            Close_Lambda := Read_Form;  -- the ) at the end of the lambda
            return New_Lambda_Mal_Type (Params, Expr);
         end;

      else

         declare
            List_SP : Mal_Handle;
            List_P : List_Class_Ptr;
            Close : String (1..1) := (1 => Types.Closing (LT));
         begin
            case LT is
               when List_List =>
                  List_SP := New_List_Mal_Type (List_Type => LT);
               when Vector_List =>
                  List_SP := Vector.New_Vector_Mal_Type;
               when Hashed_List =>
                  List_SP := Hash_Map.New_Hash_Map_Mal_Type;
            end case;


            -- Need to append to a variable so...
            List_P := Deref_List_Class (List_SP);
            loop
               if Is_Null (MTA) then
                  return New_Error_Mal_Type (Str => "expected '" & Close & "'");
               end if;
               exit when Deref (MTA).Sym_Type = Atom and then
                          Atom_Mal_Type (Deref (MTA).all).Get_Atom = Close;
               Append (List_P.all, MTA);
               MTA := Read_Form;
            end loop;
            return List_SP;
         end;
      end if;

   end Read_List;


   function Read_Form return Types.Mal_Handle is
      MTS : Mal_Handle;
   begin

      MTS := Get_Token;

      if Is_Null (MTS) then
         return Smart_Pointers.Null_Smart_Pointer;
      end if;

      if Deref (MTS).Sym_Type = Atom then

         declare
            Symbol : String := Atom_Mal_Type (Deref (MTS).all).Get_Atom;
         begin
            -- Listy things and quoting...
            if Symbol = "(" then
               return Read_List (List_List);
            elsif Symbol = "[" then
               return Read_List (Vector_List);
            elsif Symbol = "{" then
               return Read_List (Hashed_List);
            elsif Symbol = "^" then
               declare
                  Meta, Obj : Mal_Handle;
               begin
                  Meta := Read_Form;
                  Obj := Read_Form;
                  declare
                     MT : Mal_Ptr := Deref (Obj);
                  begin
                     Set_Meta (MT.all, Meta);
                  end;
                  return Obj;
               end;
            elsif Symbol = ACL.Apostrophe & "" then

               declare
                  List_SP : Mal_Handle;
                  List_P : List_Ptr;
               begin
                  List_SP := New_List_Mal_Type (List_Type => List_List);
                  List_P := Deref_List (List_SP);
                  Append (List_P.all, New_Atom_Mal_Type ("quote"));
                  Append (List_P.all, Read_Form);
                  return List_SP;
               end;

            elsif Symbol = ACL.Grave & "" then

               declare
                  List_SP : Mal_Handle;
                  List_P : List_Ptr;
               begin
                  List_SP := New_List_Mal_Type (List_Type => List_List);
                  List_P := Deref_List (List_SP);
                  Append (List_P.all, New_Atom_Mal_Type ("quasiquote"));
                  Append (List_P.all, Read_Form);
                  return List_SP;
               end;

            elsif Symbol = ACL.Tilde & "" then

               declare
                  List_SP : Mal_Handle;
                  List_P : List_Ptr;
               begin
                  List_SP := New_List_Mal_Type (List_Type => List_List);
                  List_P := Deref_List (List_SP);
                  Append (List_P.all, New_Atom_Mal_Type ("unquote"));
                  Append (List_P.all, Read_Form);
                  return List_SP;
               end;

            elsif Symbol = ACL.Commercial_At & "" then
               return New_Unitary_Mal_Type (Func => Deref, Op => Read_Form);
            else
               return MTS;
            end if;
         end;

      elsif Deref(MTS).Sym_Type = Unitary and then
            Unitary_Mal_Type (Deref (MTS).all).Get_Func = Splice_Unquote then

         declare
            List_SP : Mal_Handle;
            List_P : List_Ptr;
         begin
            List_SP := New_List_Mal_Type (List_Type => List_List);
            List_P := Deref_List (List_SP);
            Append (List_P.all, New_Atom_Mal_Type ("splice-unquote"));
            Append (List_P.all, Read_Form);
            return List_SP;
         end;

      else
         return MTS;
      end if;

   end Read_Form;

   procedure Lex_Init (S : String) is
   begin
      Str_Len := S'Length;
      Saved_Line (1..S'Length) := S;
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
