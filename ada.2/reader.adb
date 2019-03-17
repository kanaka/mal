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
with Types.Symbols.Names;

package body Reader is

   Debug : constant Boolean := Ada.Environment_Variables.Exists ("dbg_reader");

   use Types;
   use type Ada.Strings.Maps.Character_Set;

   Ignored_Set : constant Ada.Strings.Maps.Character_Set
     := Ada.Strings.Maps.Constants.Control_Set
     or Ada.Strings.Maps.To_Set (" ,;");

   Symbol_Set : constant Ada.Strings.Maps.Character_Set
     := not (Ignored_Set or Ada.Strings.Maps.To_Set ("""'()@[]^`{}~"));

   function Read_Str (Source : in String) return Types.Mal.T_Array is

      I : Positive := Source'First;
      --  Index in Source of the currently read character.

      --  Big arrays on the stack are faster than repeated dynamic
      --  reallocations. This single buffer is used by all Read_List
      --  recursive invocations, and by Read_Str.
      Buffer : Mal.T_Array (1 .. Source'Length);
      B_Last : Natural := Buffer'First - 1;
      --  Index in Buffer of the currently written MAL expression.

      procedure Read_Form;
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
      --  Increment I until it exceeds Source'Last or stops
      --  designating an allowed character.

      --  Read_Atom has been merged into the same case/switch
      --  statement, for clarity and efficiency.
      procedure Read_List (Ending      : in Character;
                           Constructor : in not null Mal.Builtin_Ptr)
        with Inline;
      procedure Read_Quote (Symbol : in Symbols.Ptr) with Inline;
      procedure Read_String with Inline;
      procedure Read_With_Meta with Inline;

      ----------------------------------------------------------------------

      procedure Read_List (Ending      : in Character;
                           Constructor : in not null Mal.Builtin_Ptr) is
         Opening : constant Character := Source (I);
         B_First : constant Positive := B_Last;
      begin
         I := I + 1;         --  Skip (, [ or {.
         loop
            Skip_Ignored;
            Err.Check (I <= Source'Last, "unbalanced '" & Opening & "'");
            exit when Source (I) = Ending;
            Read_Form;
            B_Last := B_Last + 1;
         end loop;
         I := I + 1;         --  Skip ), ] or }.
         Buffer (B_First) :=  Constructor.all (Buffer (B_First .. B_Last - 1));
         B_Last := B_First;
      end Read_List;

      procedure Read_Quote (Symbol : in Symbols.Ptr) is
      begin
         Buffer (B_Last) := (Kind_Symbol, Symbol);
         I := I + 1;             --  Skip the initial ' or similar.
         Skip_Ignored;
         Err.Check (I <= Source'Last, "Incomplete '" & Symbol.To_String & "'");
         B_Last := B_Last + 1;
         Read_Form;
         B_Last := B_Last - 1;
         Buffer (B_Last) := Sequences.List (Buffer (B_Last .. B_Last + 1));
      end Read_Quote;

      procedure Read_Form is
         --  After I has been increased, current token is be
         --  Source (F .. I - 1).
         F : Positive;
      begin
         case Source (I) is
            when ')' | ']' | '}' =>
               Err.Raise_With ("unbalanced '" & Source (I) & "'");
            when '"' =>
               Read_String;
            when ':' =>
               I := I + 1;
               F := I;
               Skip_Symbol;
               Buffer (B_Last) := (Kind_Keyword,
                                   Ada.Strings.Unbounded.To_Unbounded_String
                                     (Source (F .. I - 1)));
            when '-' =>
               F := I;
               Skip_Digits;
               if F + 1 < I then
                  Buffer (B_Last) := (Kind_Number,
                                      Integer'Value (Source (F .. I - 1)));
               else
                  Skip_Symbol;
                  Buffer (B_Last) := (Kind_Symbol,
                     Symbols.Constructor (Source (F .. I - 1)));
               end if;
            when '~' =>
               if I < Source'Last and then Source (I + 1) = '@' then
                  I := I + 1;
                  Read_Quote (Symbols.Names.Splice_Unquote);
               else
                  Read_Quote (Symbols.Names.Unquote);
               end if;
            when '0' .. '9' =>
               F := I;
               Skip_Digits;
               Buffer (B_Last) := (Kind_Number,
                                   Integer'Value (Source (F .. I - 1)));
            when ''' =>
               Read_Quote (Symbols.Names.Quote);
            when '`' =>
               Read_Quote (Symbols.Names.Quasiquote);
            when '@' =>
               Read_Quote (Symbols.Names.Deref);
            when '^' =>
               Read_With_Meta;
            when '(' =>
               Read_List (')', Sequences.List'Access);
            when '[' =>
               Read_List (']', Sequences.Vector'Access);
            when '{' =>
               Read_List ('}', Maps.Hash_Map'Access);
            when others =>
               F := I;
               Skip_Symbol;
               if Source (F .. I - 1) = "false" then
                  Buffer (B_Last) := (Kind_Boolean, False);
               elsif Source (F .. I - 1) = "nil" then
                  Buffer (B_Last) := Mal.Nil;
               elsif Source (F .. I - 1) = "true" then
                  Buffer (B_Last) := (Kind_Boolean, True);
               else
                  Buffer (B_Last) := (Kind_Symbol,
                     Symbols.Constructor (Source (F .. I - 1)));
               end if;
         end case;
         if Debug then
            Ada.Text_IO.Put ("reader: ");
            Ada.Text_IO.Unbounded_IO.Put_Line (Printer.Pr_Str (Buffer
                                                                 (B_Last)));
         end if;
      end Read_Form;

      procedure Read_String is
         use Ada.Strings.Unbounded;
      begin
         Buffer (B_Last) := (Kind_String, Null_Unbounded_String);
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
                        Append (Buffer (B_Last).S, Source (I));
                     when 'n' =>
                        Append (Buffer (B_Last).S, Ada.Characters.Latin_1.LF);
                     when others =>
                        Append (Buffer (B_Last).S, Source (I - 1 .. I));
                  end case;
               when others =>
                  Append (Buffer (B_Last).S, Source (I));
            end case;
         end loop;
         I := I + 1;                    --  Skip closing double quote.
      end Read_String;

      procedure Read_With_Meta is
      begin
         I := I + 1;                    --  Skip the initial ^.
         for Argument in 1 .. 2 loop
            Skip_Ignored;
            Err.Check (I <= Source'Last, "Incomplete 'with-meta'");
            Read_Form;
            B_Last := B_Last + 1;
         end loop;
         --  Replace (metadata data) with (with-meta data metadata).
         B_Last := B_Last - 2;
         Buffer (B_Last + 2) := Buffer (B_Last);
         Buffer (B_Last) := (Kind_Symbol, Symbols.Names.With_Meta);
         Buffer (B_Last) := Sequences.List (Buffer (B_Last .. B_Last + 2));
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

   begin                                --  Read_Str
      loop
         Skip_Ignored;
         exit when Source'Last < I;
         B_Last := B_Last + 1;
         Read_Form;
      end loop;
      return Buffer (Buffer'First .. B_Last);
   end Read_Str;

end Reader;
