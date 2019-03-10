with Ada.Characters.Handling;
with Ada.Characters.Latin_1;
with Ada.Strings.Maps.Constants;
with Ada.Strings.Unbounded;

with Types.Lists;
with Types.Maps;
with Types.Symbols.Names;

package body Reader is

   use Types;
   use type Ada.Strings.Maps.Character_Set;

   Ignored_Set : constant Ada.Strings.Maps.Character_Set
     := Ada.Strings.Maps.Constants.Control_Set
     or Ada.Strings.Maps.To_Set (" ,;");

   Symbol_Set : constant Ada.Strings.Maps.Character_Set
     := not (Ignored_Set or Ada.Strings.Maps.To_Set ("""'()@[]^`{}~"));

   function Read_Str (Source : in String) return Types.Mal.T is

      I : Positive := Source'First;
      --  Index of the currently considered character.

      function Read_Form return Mal.T;
      --  The recursive part of Read_Str.

      --  Helpers for Read_Form:

      procedure Skip_Ignored with Inline;
      --  Check if the current character is ignorable or a comment.
      --  Increment I until it exceeds Source'Last or designates
      --  an interesting character.

      procedure Skip_Digits with Inline;
      --  Increment I at least once, until I exceeds Source'Last or
      --  designates something else than a decimal digit.

      procedure Skip_Symbol with Inline;
      --  Check if the current character is allowed in a symbol name.
      --  Increment I uuntil it exceeds Source'Last or stops
      --  designating an allowed character.

      --  Read_Atom has been merged into the same case/switch
      --  statement, for clarity and efficiency.
      function Read_List (Ending : in Character) return Mal.T_Array
        with Inline;
      function Read_Quote (Symbol : in Symbols.Ptr) return Mal.T with Inline;
      function Read_String return Mal.T with Inline;
      function Read_With_Meta return Mal.T with Inline;

      ----------------------------------------------------------------------

      function Read_List (Ending : in Character) return Mal.T_Array is
         --  Big arrays on the stack are faster than repeated
         --  dynamic reallocations.
         Opening : constant Character := Source (I);
         Buffer : Mal.T_Array (I + 1 .. Source'Last);
         B_Last : Natural := I;
      begin
         I := I + 1;         --  Skip (, [ or {.
         loop
            Skip_Ignored;
            if Source'Last < I then
               raise Reader_Error with "unbalanced '" & Opening & "'";
            end if;
            exit when Source (I) = Ending;
            B_Last := B_Last + 1;
            Buffer (B_Last) := Read_Form;
         end loop;
         I := I + 1;         --  Skip ), ] or }.
         return Buffer (Buffer'First .. B_Last);
      end Read_List;

      function Read_Quote (Symbol : in Symbols.Ptr) return Mal.T is
      begin
         I := I + 1;             --  Skip the initial ' or similar.
         Skip_Ignored;
         if Source'Last < I then
            raise Reader_Error with "Incomplete '" & Symbol.To_String & "'";
         end if;
         return Lists.List (Mal.T_Array'((Kind_Symbol, Symbol), Read_Form));
      end Read_Quote;

      function Read_Form return Mal.T is
         F : Positive;
      begin
         case Source (I) is
            when ')' | ']' | '}' =>
               raise Reader_Error with "unbalanced '" & Source (I) & "'";
            when '"' =>
               return Read_String;
            when ':' =>
               I := I + 1;
               F := I;
               Skip_Symbol;
               return (Kind_Keyword, Ada.Strings.Unbounded.To_Unbounded_String
                         (Source (F .. I - 1)));
            when '-' =>
               F := I;
               Skip_Digits;
               if F + 1 < I then
                  return (Kind_Number, Integer'Value (Source (F .. I - 1)));
               end if;
               Skip_Symbol;
               return (Kind_Symbol, Symbols.Constructor (Source (F .. I - 1)));
            when '~' =>
               if I < Source'Last and then Source (I + 1) = '@' then
                  I := I + 1;
                  return Read_Quote (Symbols.Names.Splice_Unquote);
               end if;
               return Read_Quote (Symbols.Names.Unquote);
            when '0' .. '9' =>
               F := I;
               Skip_Digits;
               return (Kind_Number, Integer'Value (Source (F .. I - 1)));
            when ''' =>
               return Read_Quote (Symbols.Names.Quote);
            when '`' =>
               return Read_Quote (Symbols.Names.Quasiquote);
            when '@' =>
               return Read_Quote (Symbols.Names.Deref);
            when '^' =>
               return Read_With_Meta;
            when '(' =>
               return Lists.List (Read_List (')'));
            when '[' =>
               return Lists.Vector (Read_List (']'));
            when '{' =>
               return Maps.Hash_Map (Read_List ('}'));
            when others =>
               F := I;
               Skip_Symbol;
               if Source (F .. I - 1) = "false" then
                  return (Kind_Boolean, False);
               elsif Source (F .. I - 1) = "nil" then
                  return Mal.Nil;
               elsif Source (F .. I - 1) = "true" then
                  return (Kind_Boolean, True);
               end if;
               return (Kind_Symbol, Symbols.Constructor (Source (F .. I - 1)));
         end case;
      end Read_Form;

      function Read_String return Mal.T is
         use Ada.Strings.Unbounded;
         S : Unbounded_String;
      begin
         loop
            I := I + 1;
            if Source'Last < I then
               raise Reader_Error with "unbalanced '""'";
            end if;
            case Source (I) is
               when '"' =>
                  exit;
               when '\' =>
                  I := I + 1;
                  if Source'Last < I then
                     raise Reader_Error with "unbalanced '""'";
                  end if;
                  case Source (I) is
                     when '\' | '"' =>
                        Append (S, Source (I));
                     when 'n' =>
                        Append (S, Ada.Characters.Latin_1.LF);
                     when others =>
                        Append (S, Source (I - 1 .. I));
                  end case;
               when others =>
                  Append (S, Source (I));
            end case;
         end loop;
         I := I + 1;                    --  Skip closing double quote.
         return (Kind_String, S);
      end Read_String;

      function Read_With_Meta return Mal.T is
         Args : Mal.T_Array (1 .. 3);
      begin
         Args (1) := (Kind_Symbol, Symbols.Names.With_Meta);

         I := I + 1;                 --  Skip the initial ^.

         Skip_Ignored;
         if Source'Last < I then
            raise Reader_Error with "incomplete 'with-meta'";
         end if;
         Args (3) := Read_Form;

         Skip_Ignored;
         if Source'Last < I then
            raise Reader_Error with "incomplete 'with-meta'";
         end if;
         Args (2) := Read_Form;

         return Lists.List (Args);
      end Read_With_Meta;

      procedure Skip_Digits is
         use Ada.Characters.Handling;
      begin
         loop
            I := I + 1;
            exit when Source'Last < I or else not Is_Digit (Source (I));
         end loop;
      end Skip_Digits;

      procedure Skip_Ignored is
         use Ada.Characters.Handling;
         use Ada.Strings.Maps;
      begin
         Ignored : while I <= Source'Last
                         and then Is_In (Source (I), Ignored_Set)
         loop
            if Source (I) = ';' then
               Comment : loop
                  I := I + 1;
                  exit Ignored when Source'Last < I;
                  exit Comment when Is_Line_Terminator (Source (I));
               end loop Comment;
            end if;
            I := I + 1;
         end loop Ignored;
      end Skip_Ignored;

      procedure Skip_Symbol is
         use Ada.Strings.Maps;
      begin
         while I <= Source'Last and then Is_In (Source (I), Symbol_Set) loop
            I := I + 1;
         end loop;
      end Skip_Symbol;

      ----------------------------------------------------------------------

      Result : Mal.T;
   begin                                --  Read_Str
      Skip_Ignored;
      if Source'Last < I then
         raise Empty_Source with "attempting to read an empty line";
      end if;
      Result := Read_Form;
      Skip_Ignored;
      if I <= Source'Last then
         raise Reader_Error
           with "unexpected characters '" & Source (I .. Source'Last)
           & "' after '" & Source (Source'First .. I - 1) & ''';
      end if;
      return Result;
   end Read_Str;

end Reader;
