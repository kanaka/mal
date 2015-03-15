with Ada.IO_Exceptions;
with Ada.Characters.Latin_1;
with Ada.Strings.Maps.Constants;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Opentoken.Recognizer.Character_Set;
with Opentoken.Recognizer.Identifier;
with Opentoken.Recognizer.Integer;
with Opentoken.Recognizer.Keyword;
with Opentoken.Recognizer.Line_Comment;
with Opentoken.Recognizer.Separator;
with Opentoken.Recognizer.Single_Character_Set;
with Opentoken.Recognizer.String;
with OpenToken.Text_Feeder.String;
with Opentoken.Token.Enumerated.Analyzer;

package body Reader is

   type Lexemes is (Int, Sym,
                    Nil, True_Tok, False_Tok, Exp_Tok,
                    Str, Atom,
                    Whitespace, Comment);

   package Lisp_Tokens is new Opentoken.Token.Enumerated (Lexemes);
   package Tokenizer is new Lisp_Tokens.Analyzer;

   Exp_Recognizer   : constant Tokenizer.Recognizable_Token :=
     Tokenizer.Get(Opentoken.Recognizer.Separator.Get ("**"));

   Nil_Recognizer   : constant Tokenizer.Recognizable_Token :=
     Tokenizer.Get(Opentoken.Recognizer.Keyword.Get ("nil"));

   True_Recognizer : constant Tokenizer.Recognizable_Token :=
     Tokenizer.Get(Opentoken.Recognizer.Keyword.Get ("true"));

   False_Recognizer : constant Tokenizer.Recognizable_Token :=
     Tokenizer.Get (Opentoken.Recognizer.Keyword.Get ("false"));

   ID_Recognizer   : constant Tokenizer.Recognizable_Token :=
     Tokenizer.Get(Opentoken.Recognizer.Identifier.Get);

   Int_Recognizer  : constant Tokenizer.Recognizable_Token :=
     Tokenizer.Get(Opentoken.Recognizer.Integer.Get);

   -- Use the C style for escaped strings.
   String_Recognizer : constant Tokenizer.Recognizable_Token :=
     Tokenizer.Get
       (Opentoken.Recognizer.String.Get
         (Escapeable => True, 
          Double_Delimiter => False));

   -- Atom definition
   Start_Chars : Ada.Strings.Maps.Character_Set :=
     Ada.Strings.Maps.Constants.Letter_Set;

   Body_Chars : Ada.Strings.Maps.Character_Set :=
     Ada.Strings.Maps."or"
       (Ada.Strings.Maps.Constants.Alphanumeric_Set,
        Ada.Strings.Maps.To_Set ('-'));

   Atom_Recognizer  : constant Tokenizer.Recognizable_Token :=
     Tokenizer.Get
       (Opentoken.Recognizer.Identifier.Get (Start_Chars, Body_Chars));

   Lisp_Syms : constant Ada.Strings.Maps.Character_Set :=
     Ada.Strings.Maps.To_Set ("[]{}()'`~^@+-*/");

   Sym_Recognizer : constant Tokenizer.Recognizable_Token :=
     Tokenizer.Get (Opentoken.Recognizer.Single_Character_Set.Get (Lisp_Syms));

   Lisp_Whitespace : constant Ada.Strings.Maps.Character_Set :=
     Ada.Strings.Maps.To_Set (Ada.Characters.Latin_1.HT &
                              Ada.Characters.Latin_1.Space &
                              Ada.Characters.Latin_1.Comma);

   Whitesp_Recognizer : constant Tokenizer.Recognizable_Token :=
     Tokenizer.Get (Opentoken.Recognizer.Character_Set.Get (Lisp_Whitespace));

   Comment_Recognizer  : constant Tokenizer.Recognizable_Token :=
     Tokenizer.Get(Opentoken.Recognizer.Line_Comment.Get (";"));

   Syntax : constant Tokenizer.Syntax :=
     (Int           => Int_Recognizer,
      Sym => Sym_Recognizer,
      Nil           => Nil_Recognizer,
      True_Tok      => True_Recognizer,
      False_Tok     => False_Recognizer,
      Exp_Tok       => Exp_Recognizer,
      Str           => String_Recognizer,
      Atom          => Atom_Recognizer,
      Whitespace    => Whitesp_Recognizer,
      Comment       => Comment_Recognizer  --,
     );

   Input_Feeder : aliased OpenToken.Text_Feeder.String.Instance;

   Analyzer : Tokenizer.Instance :=
     Tokenizer.Initialize (Syntax, Input_Feeder'access);


   function Get_Token_String return String is
   begin
      return Tokenizer.Lexeme (Analyzer);
   end Get_Token_String;


   function Get_Token_Char return Character is
      S : String := Tokenizer.Lexeme (Analyzer);
   begin
      return S(S'First);
   end Get_Token_Char;


   function Get_Token return Types.Mal_Type_Access is
      Res : Types.Mal_Type_Access;
   begin
      Tokenizer.Find_Next (Analyzer);
      case Tokenizer.ID (Analyzer) is
         when Int =>
            Res := new Types.Mal_Type'
              (Sym_Type => Types.Int,
               Int_Val => Integer'Value (Get_Token_String));
         when Sym =>
            Res := new Types.Mal_Type'
              (Sym_Type => Types.Sym, Symbol => Get_Token_Char);
         when Nil =>
            Res := new Types.Mal_Type'
              (Sym_Type => Types.Atom,
               The_Atom => Ada.Strings.Unbounded.To_Unbounded_String
                        (Get_Token_String));
         when True_Tok =>
            Res := new Types.Mal_Type'
              (Sym_Type => Types.Atom,
               The_Atom => Ada.Strings.Unbounded.To_Unbounded_String
                        (Get_Token_String));
         when False_Tok =>
            Res := new Types.Mal_Type'
              (Sym_Type => Types.Atom,
               The_Atom => Ada.Strings.Unbounded.To_Unbounded_String
                        (Get_Token_String));
         when Exp_Tok =>
            Res := new Types.Mal_Type'
              (Sym_Type => Types.Atom,
               The_Atom => Ada.Strings.Unbounded.To_Unbounded_String
                        (Get_Token_String));
         when Str =>
            Res := new Types.Mal_Type'
              (Sym_Type => Types.Str,
               The_String => Ada.Strings.Unbounded.To_Unbounded_String
                        (Get_Token_String));
         when Atom =>
            Res := new Types.Mal_Type'
              (Sym_Type => Types.Atom,
               The_Atom => Ada.Strings.Unbounded.To_Unbounded_String
                        (Get_Token_String));
         when Whitespace | Comment => null;
      end case;
      return Res;
   end Get_Token;


   -- Parsing
   function Read_Form return Types.Mal_Type_Access;

   function Read_List return Types.Mal_Type_Access is
      use types;
      List_MT, MTA : Types.Mal_Type_Access;
   begin
      List_MT := new Types.Mal_Type'
                       (Sym_Type => Types.List, 
                        The_List => Types.Lists.Empty_List);
      loop
         MTA := Read_Form;
         exit when MTA = null or else
                   MTA.all = (Sym_Type => Sym, Symbol => ')');
         Types.Lists.Append (List_MT.The_List, MTA);
      end loop;
      return List_MT;
   end Read_List;


   function Read_Form return Types.Mal_Type_Access is
      use Types;
      MT : Types.Mal_Type_Access;
   begin
      MT := Get_Token;
      if MT.all = (Sym_Type => Sym, Symbol => '(') then
         return Read_List;
      else
         return MT;
      end if;
   end Read_Form;


   function Read_Str (S : String) return Types.Mal_Type_Access is
   begin
      Analyzer.Reset;
      Input_Feeder.Set (S);
      return Read_Form;
   exception
      when OPENTOKEN.SYNTAX_ERROR =>
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            "Lexical error at char " & Integer'Image (Analyzer.Line));
         raise Ada.IO_Exceptions.End_Error;
   end Read_Str;
   

end Reader;
