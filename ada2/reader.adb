with Ada.Characters.Latin_1;
with Ada.Strings.Unbounded;

with Types.Lists;
with Types.Maps;
with Types.Symbols.Names;

package body Reader is

   function Read_Str (Source : in String) return Types.Mal.T is

      use Types;

      First : Positive;
      Last  : Natural := Source'First - 1;

      function Read_Form return Mal.T;
      --  The recursive part of Read_Str.

      procedure Find_Next_Token;
      --  Search next token from index Last + 1.
      --  If none is found, set First to Source'Last + 1.
      --  Find_Next_Token is normally invoked right before Read_Form,
      --  allowing the caller to check whether First <= Source'Last.

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
                        raise Reader_Error with "unbalanced '""'";
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

      function Read_Form return Mal.T is

         --  Read_Atom has been merged into the same case/switch
         --  statement, for clarity and efficiency.
         function Read_List (Ending : in Character) return Mal.T_Array
           with Inline;
         function Read_Quote (Symbol : in Symbols.Ptr) return Mal.T
           with Inline;

         function Read_List (Ending : in Character) return Mal.T_Array is
            --  Using big arrays on the stack is faster than doing
            --  repeated dynamic reallocations.
            Buffer : Mal.T_Array (First + 1 .. Source'Last);
            B_Last : Natural := Buffer'First - 1;
         begin
            loop
               Find_Next_Token;
               if Source'Last < First then
                  raise Reader_Error with "unbalanced '" & Ending & "'";
               end if;
               exit when Source (First) = Ending;
               B_Last := B_Last + 1;
               Buffer (B_Last) := Read_Form;
            end loop;
            return Buffer (Buffer'First .. B_Last);
         end Read_List;

         function Read_Quote (Symbol : in Symbols.Ptr) return Mal.T is
         begin
            Find_Next_Token;
            if Source'Last < First then
               raise Reader_Error with "Unfinished '" & Symbol.To_String & "'";
            end if;
            return Lists.List (Mal.T_Array'((Kind_Symbol, Symbol), Read_Form));
         end Read_Quote;

         use Ada.Strings.Unbounded;
      begin                             --  Read_Form.
         case Source (First) is
            when '(' =>
               return Lists.List (Read_List (')'));
            when '[' =>
               return Lists.Vector (Read_List (']'));
            when '{' =>
               return Maps.Hash_Map (Read_List ('}'));
            when '"' =>
               declare
                  Buffer : Unbounded_String;
                  I      : Positive := First + 1;
               begin
                  while I < Last loop
                     if Source (I) /= '\' or else I + 1 = Last then
                        Append (Buffer, Source (I));
                     else
                        case Source (I + 1) is
                           when '\' | '"' =>
                              I := I + 1;
                              Append (Buffer, Source (I));
                           when 'n' =>
                              I := I + 1;
                              Append (Buffer, Ada.Characters.Latin_1.LF);
                           when others =>
                              Append (Buffer, Source (I));
                        end case;
                     end if;
                     I := I + 1;
                  end loop;
                  return (Kind_String, Buffer);
               end;
            when ':' =>
               return (Kind_Keyword,
                       To_Unbounded_String (Source (First + 1 .. Last)));
            when '-' =>
               if First < Last
                 and then (for all C of Source (First + 1 .. Last) =>
                             C in '0' .. '9')
               then
                  return (Kind_Number, Integer'Value (Source (First .. Last)));
               else
                  return (Kind_Symbol,
                          Symbols.Constructor (Source (First .. Last)));
               end if;
            when '0' .. '9' =>
               return (Kind_Number, Integer'Value (Source (First .. Last)));
            when ''' =>
               return Read_Quote (Symbols.Names.Quote);
            when '`' =>
               return Read_Quote (Symbols.Names.Quasiquote);
            when '@' =>
               return Read_Quote (Symbols.Names.Deref);
            when '~' =>
               if First = Last then
                  return Read_Quote (Symbols.Names.Unquote);
               else
                  return Read_Quote (Symbols.Names.Splice_Unquote);
               end if;
            when '^' =>
               declare
                  Args : Mal.T_Array (1 .. 3);
               begin
                  Args (1) := (Kind_Symbol, Symbols.Names.With_Meta);
                  Find_Next_Token;
                  if Source'Last < First then
                     raise Reader_Error with "Unfinished 'with-meta'";
                  end if;
                  Args (3) := Read_Form;
                  Find_Next_Token;
                  if Source'Last < First then
                     raise Reader_Error with "Unfinished 'with-meta'";
                  end if;
                  Args (2) := Read_Form;
                  return Lists.List (Args);
               end;
            when others =>
               if Source (First .. Last) = "false" then
                  return (Kind_Boolean, False);
               elsif Source (First .. Last) = "nil" then
                  return Mal.Nil;
               elsif Source (First .. Last) = "true" then
                  return (Kind_Boolean, True);
               else
                  return (Kind_Symbol,
                          Symbols.Constructor (Source (First .. Last)));
               end if;
         end case;
      end Read_Form;

      ----------------------------------------------------------------------

   begin
      Find_Next_Token;
      if Source'Last < First then
         raise Empty_Source with "attempting to read an empty line";
      end if;
      return Read_Form;
   end Read_Str;

end Reader;
