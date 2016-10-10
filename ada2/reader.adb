with Ada.Characters.Latin_1;
with Atoms;
with Lists;
with Maps;
with Names;
with Strings;

package body Reader is

   function Read_Str (Source : in String) return Types.Mal_Type
   is
      First : Positive;
      Last  : Natural := Source'First - 1;

      function Read_Form return Types.Mal_Type;

      procedure Find_Next_Token;
      --  Search next token from index Last + 1.
      --  If none is found, set First to Source'Last + 1.
      --  Find_Next_Token is normally invoked right before Read_Form,
      --  allowing the caller to check whether First <= Source'Last.

      --  Helpers:

      --  Read_Atom has been merged into the same case/switch
      --  statement, for clarity and efficiency.
      function Read_List (Ending : in Character) return Types.Mal_Type_Array
        with Inline;
      function Read_Quote (Symbol : in Strings.Ptr) return Types.Mal_Type
        with Inline;

      ----------------------------------------------------------------------
      procedure Find_Next_Token
      is
         use Ada.Characters.Latin_1;
      begin
         First := Last + 1;
         while First <= Source'Last loop

            case Source (First) is

               when ' ' | ',' | HT | VT | LF | CR =>
                  First := First + 1;

               when '[' | ']' | '{' | '}' | '(' | ')' | ''' | '`' | '^' | '@'
                 =>
                  Last := First;
                  exit;

               when '~' =>
                  if First + 1 <= Source'Last
                    and then Source (First + 1) = '@'
                  then
                     Last := First + 1;
                  else
                     Last := First;
                  end if;
                  exit;

               when '"' =>
                  Last := First + 1;
                  loop
                     if Source'Last < Last then
                        raise Reader_Error with "expected '""'";
                     end if;
                     exit when Source (Last) = '"';
                     if Source (Last) = '\' then
                        Last := Last + 1;
                     end if;
                     Last := Last + 1;
                  end loop;
                  exit;

               when ';' =>
                  First := First + 1;
                  while First <= Source'Last loop
                     if Source (First) = LF then
                        First := First + 1;
                        exit;
                     end if;
                     First := First + 1;
                  end loop;

               when others =>
                  Last := First;
                  while Last + 1 <= Source'Last
                    and then Source (Last + 1) not in
                    ' ' | ',' | HT | VT | LF | CR | '[' | ']' | '{' | '}'
                    | '(' | ')' | ''' | '`' | '^' | '@' | '~' | '"'  | ';'
                  loop
                     Last := Last + 1;
                  end loop;
                  exit;

            end case;
         end loop;
      end Find_Next_Token;

      function Read_Form return Types.Mal_Type
      is
         use Types;
      begin
         case Source (First) is

            when '(' =>
               return (Kind_List, Atoms.No_Element,
                       Lists.Alloc (Read_List (')')));
            when '[' =>
               return (Kind_Vector, Atoms.No_Element,
                       Lists.Alloc (Read_List (']')));
            when '{' =>
               return (Kind_Map, Atoms.No_Element,
                       Maps.Hash_Map (Read_List ('}')));

            when '"' =>
               declare
                  Buffer : String (First .. Last);
                  B_Last : Natural := Buffer'First - 1;
                  I      : Positive := First + 1;
               begin
                  while I <= Last - 1 loop
                     if Source (I) /= '\' or else I = Last - 1 then
                        B_Last := B_Last + 1;
                        Buffer (B_Last) := Source (I);
                     else
                        case Source (I + 1) is
                           when '\' | '"' =>
                              B_Last := B_Last + 1;
                              Buffer (B_Last) := Source (I + 1);
                              I := I + 1;
                           when 'n' =>
                              B_Last := B_Last + 1;
                              Buffer (B_Last) := Ada.Characters.Latin_1.LF;
                              I := I + 1;
                           when others =>
                              B_Last := B_Last + 1;
                              Buffer (B_Last) := Source (I);
                        end case;
                     end if;
                     I := I + 1;
                  end loop;
                  return (Kind_String, Atoms.No_Element,
                          Strings.Alloc (Buffer (Buffer'First .. B_Last)));
               end;
            when ':' =>
               return (Kind_Keyword, Atoms.No_Element,
                       Strings.Alloc (Source (First + 1 .. Last)));

            when '-' =>
               if First < Last
                 and then (for all C of Source (First + 1 .. Last) =>
                             C in '0' .. '9')
               then
                  return (Kind_Number, Atoms.No_Element,
                          Integer'Value (Source (First .. Last)));
               else
                  return (Kind_Symbol, Atoms.No_Element,
                          Strings.Alloc (Source (First .. Last)));
               end if;
            when '0' .. '9' =>
               return (Kind_Number, Atoms.No_Element,
                       Integer'Value (Source (First .. Last)));

            when ''' =>
               return Read_Quote (Names.Quote);
            when '`' =>
               return Read_Quote (Names.Quasiquote);
            when '@' =>
               return Read_Quote (Names.Deref);
            when '~' =>
               if First = Last then
                  return Read_Quote (Names.Unquote);
               else
                  return Read_Quote (Names.Splice_Unquote);
               end if;
            when '^' =>
               return Result : constant Mal_Type
                 := (Kind_List, Atoms.No_Element, Lists.Alloc (3))
               do
                  Result.L.Replace_Element (1, Mal_Type'
                     (Kind_Symbol, Atoms.No_Element, Names.With_Meta));
                  Find_Next_Token;
                  if Source'Last < First then
                     raise Reader_Error with "Unfinished 'with-meta'";
                  end if;
                  Result.L.Replace_Element (3, Read_Form);
                  Find_Next_Token;
                  if Source'Last < First then
                     raise Reader_Error with "Unfinished 'with-meta'";
                  end if;
                  Result.L.Replace_Element (2, Read_Form);
               end return;

            when others =>
               if Source (First .. Last) = "nil" then
                  return (Kind_Nil, Atoms.No_Element);
               elsif Source (First .. Last) = "true" then
                  return (Kind_Boolean, Atoms.No_Element, True);
               elsif Source (First .. Last) = "false" then
                  return (Kind_Boolean, Atoms.No_Element, False);
               else
                  return (Kind_Symbol, Atoms.No_Element,
                          Strings.Alloc (Source (First .. Last)));
               end if;
         end case;
      end Read_Form;

      function Read_List (Ending : in Character) return Types.Mal_Type_Array
      is
         --  Using big arrays on the stack is faster than doing
         --  repeated dynamic reallocations.
         Buffer : Types.Mal_Type_Array (First + 1 .. Source'Last);
         B_Last : Natural := Buffer'First - 1;
      begin
         loop
            Find_Next_Token;
            if Source'Last < First then
               raise Reader_Error with "expected '" & Ending & "'";
            end if;
            exit when Source (First) = Ending;
            B_Last := B_Last + 1;
            Buffer (B_Last) := Read_Form;
         end loop;
         return Buffer (Buffer'First .. B_Last);
      end Read_List;

      function Read_Quote (Symbol : in Strings.Ptr) return Types.Mal_Type is
         use Types;
         Result : constant Mal_Type
           := (Kind_List, Atoms.No_Element, Lists.Alloc (2));
      begin
         Result.L.Replace_Element (1,
            Mal_Type'(Kind_Symbol, Atoms.No_Element, Symbol));
         Find_Next_Token;
         if Source'Last < First then
            raise Reader_Error with "Unfinished '" & Symbol.Deref & "'";
         end if;
         Result.L.Replace_Element (2, Read_Form);
         return Result;
      end Read_Quote;

      ----------------------------------------------------------------------

   begin
      Find_Next_Token;
      if Source'Last < First then
         raise Empty_Source;
      end if;
      return Read_Form;
   end Read_Str;

end Reader;
