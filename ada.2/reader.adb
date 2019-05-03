with Ada.Characters.Handling;
with Ada.Characters.Latin_1;
with Ada.Environment_Variables;
with Ada.Strings.Maps.Constants;
with Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO;

with Err;
with Printer;
with Types.Maps;
with Types.Sequences;
with Types.Strings;

package body Reader is

   Debug : constant Boolean := Ada.Environment_Variables.Exists ("dbgread");

   use all type Types.Kind_Type;
   use all type Ada.Strings.Maps.Character_Set;

   Ignored_Set : constant Ada.Strings.Maps.Character_Set
     := Ada.Strings.Maps.Constants.Control_Set
     or To_Set (" ,;");

   Symbol_Set : constant Ada.Strings.Maps.Character_Set
     := not (Ignored_Set or To_Set ("""'()@[]^`{}~"));

   function Read_Str (Source : in String) return Types.T_Array is

      I : Positive := Source'First;
      --  Index in Source of the currently read character.

      --  Big arrays on the stack are faster than repeated dynamic
      --  reallocations. This single buffer is used by all Read_List
      --  recursive invocations, and by Read_Str.
      Buffer : Types.T_Array (1 .. Source'Length);
      B_Last : Natural := Buffer'First - 1;
      --  Index in Buffer of the currently written MAL expression.

      function Read_Form return Types.T;
      --  The recursive part of Read_Str.

      --  Helpers for Read_Form:

      procedure Skip_Ignored;
      --  Check if the current character is ignorable or a comment.
      --  Increment I until it exceeds Source'Last or designates
      --  an interesting character.

      procedure Skip_Digits with Inline;
      --  Increment I at least once, until I exceeds Source'Last or
      --  designates something else than a decimal digit.

      procedure Skip_Symbol with Inline;
      --  Check if the current character is allowed in a symbol name.
      --  Increment I until it exceeds Source'Last or stops
      --  designating an allowed character.

      --  Read_Atom has been merged into the same case/switch
      --  statement, for clarity and efficiency.

      function Read_List (Ending : in Character) return Natural;
      --  Returns the index of the last elements in Buffer.
      --  The elements have been stored in Buffer (B_Last .. result).

      function Read_Quote (Symbol : in String) return Types.T;

      function Read_String return Types.T;

      function Read_With_Meta return Types.T;

      ----------------------------------------------------------------------

      function Read_List (Ending : in Character) return Natural is
         Opening : constant Character := Source (I);
         Old     : constant Natural := B_Last;
         Result  : Positive;
      begin
         I := I + 1;         --  Skip (, [ or {.
         loop
            Skip_Ignored;
            Err.Check (I <= Source'Last, "unbalanced '" & Opening & "'");
            exit when Source (I) = Ending;
            B_Last := B_Last + 1;
            Buffer (B_Last) := Read_Form;
         end loop;
         I := I + 1;         --  Skip ), ] or }.
         Result := B_Last;
         B_Last := Old;
         return Result;
      end Read_List;

      function Read_Quote (Symbol : in String) return Types.T is
         R : constant Types.Sequence_Ptr := Types.Sequences.Constructor (2);
      begin
         I := I + 1;             --  Skip the initial ' or similar.
         R.all.Data (1) := (Kind_Symbol, Types.Strings.Alloc (Symbol));
         Skip_Ignored;
         Err.Check (I <= Source'Last, "Incomplete '" & Symbol & "'");
         R.all.Data (2) := Read_Form;
         return (Kind_List, R);
      end Read_Quote;

      function Read_Form return Types.T is
         --  After I has been increased, current token is be
         --  Source (F .. I - 1).
         F : Positive;
         R : Types.T;                   --  The result of this function.
      begin
         case Source (I) is
            when ')' | ']' | '}' =>
               Err.Raise_With ("unbalanced '" & Source (I) & "'");
            when '"' =>
               R := Read_String;
            when ':' =>
               I := I + 1;
               F := I;
               Skip_Symbol;
               R := (Kind_Keyword, Types.Strings.Alloc (Source (F .. I - 1)));
            when '-' =>
               F := I;
               Skip_Digits;
               if F + 1 < I then
                  R := (Kind_Number, Integer'Value (Source (F .. I - 1)));
               else
                  Skip_Symbol;
                  R := (Kind_Symbol,
                        Types.Strings.Alloc (Source (F .. I - 1)));
               end if;
            when '~' =>
               if I < Source'Last and then Source (I + 1) = '@' then
                  I := I + 1;
                  R := Read_Quote ("splice-unquote");
               else
                  R := Read_Quote ("unquote");
               end if;
            when '0' .. '9' =>
               F := I;
               Skip_Digits;
               R := (Kind_Number, Integer'Value (Source (F .. I - 1)));
            when ''' =>
               R := Read_Quote ("quote");
            when '`' =>
               R := Read_Quote ("quasiquote");
            when '@' =>
               R := Read_Quote ("deref");
            when '^' =>
               R := Read_With_Meta;
            when '(' =>
               R := Types.Sequences.List
                 (Buffer (B_Last + 1 .. Read_List (')')));
            when '[' =>
               R := Types.Sequences.Vector
                 (Buffer (B_Last + 1 .. Read_List (']')));
            when '{' =>
               R := Types.Maps.Hash_Map
                 (Buffer (B_Last + 1 .. Read_List ('}')));
            when others =>
               F := I;
               Skip_Symbol;
               if Source (F .. I - 1) = "false" then
                  R := (Kind_Boolean, False);
               elsif Source (F .. I - 1) = "nil" then
                  R := Types.Nil;
               elsif Source (F .. I - 1) = "true" then
                  R := (Kind_Boolean, True);
               else
                  R := (Kind_Symbol,
                        Types.Strings.Alloc (Source (F .. I - 1)));
               end if;
         end case;
         if Debug then
            Ada.Text_IO.Put ("reader: ");
            Ada.Text_IO.Unbounded_IO.Put_Line (Printer.Pr_Str (R));
         end if;
         return R;
      end Read_Form;

      function Read_String return Types.T is
         use Ada.Strings.Unbounded;
         Result : Unbounded_String;
      begin
         loop
            I := I + 1;
            Err.Check (I <= Source'Last, "unbalanced '""'");
            case Source (I) is
               when '"' =>
                  exit;
               when '\' =>
                  I := I + 1;
                  Err.Check (I <= Source'Last, "unbalanced '""'");
                  case Source (I) is
                     when '\' | '"' =>
                        Append (Result, Source (I));
                     when 'n' =>
                        Append (Result, Ada.Characters.Latin_1.LF);
                     when others =>
                        Append (Result, Source (I - 1 .. I));
                  end case;
               when others =>
                  Append (Result, Source (I));
            end case;
         end loop;
         I := I + 1;                    --  Skip closing double quote.
         return (Kind_String, Types.Strings.Alloc (To_String (Result)));
      end Read_String;

      function Read_With_Meta return Types.T is
         List : constant Types.Sequence_Ptr := Types.Sequences.Constructor (3);
      begin
         I := I + 1;                    --  Skip the initial ^.
         List.all.Data (1) := (Kind_Symbol, Types.Strings.Alloc ("with-meta"));
         for I in reverse 2 .. 3 loop
            Skip_Ignored;
            Err.Check (I <= Source'Last, "Incomplete 'with-meta'");
            List.all.Data (I) := Read_Form;
         end loop;
         return (Kind_List, List);
      end Read_With_Meta;

      procedure Skip_Digits is
         use Ada.Characters.Handling;
      begin
         loop
            I := I + 1;
            exit when Source'Last < I;
            exit when not Is_Digit (Source (I));
         end loop;
      end Skip_Digits;

      procedure Skip_Ignored is
         use Ada.Characters.Handling;
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
      begin
         while I <= Source'Last and then Is_In (Source (I), Symbol_Set) loop
            I := I + 1;
         end loop;
      end Skip_Symbol;

      ----------------------------------------------------------------------

   begin                                --  Read_Str
      loop
         Skip_Ignored;
         exit when Source'Last < I;
         B_Last := B_Last + 1;
         Buffer (B_Last) := Read_Form;
      end loop;
      return Buffer (Buffer'First .. B_Last);
   end Read_Str;

end Reader;
