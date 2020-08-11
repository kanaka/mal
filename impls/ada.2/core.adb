with Ada.Calendar;
with Ada.Characters.Latin_1;
with Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO;

with Err;
with Printer;
with Reader;
with Types.Atoms;
with Types.Builtins;
with Types.Fns;
with Types.Maps;
with Types.Sequences;
with Types.Strings;

package body Core is

   package ASU renames Ada.Strings.Unbounded;
   use all type Types.Kind_Type;

   --  Used by time_ms.
   Start_Time : constant Ada.Calendar.Time := Ada.Calendar.Clock;

   generic
      Kind : in Types.Kind_Type;
   function Generic_Kind_Test (Args : in Types.T_Array) return Types.T;
   function Generic_Kind_Test (Args : in Types.T_Array) return Types.T is
   begin
      Err.Check (Args'Length = 1, "expected 1 parameter");
      return (Kind_Boolean, Args (Args'First).Kind = Kind);
   end Generic_Kind_Test;

   generic
      with function Ada_Operator (Left, Right : in Integer) return Integer;
   function Generic_Mal_Operator (Args : in Types.T_Array) return Types.T;
   function Generic_Mal_Operator (Args : in Types.T_Array) return Types.T is
   begin
      Err.Check (Args'Length = 2
                   and then Args (Args'First).Kind = Kind_Number
                   and then Args (Args'Last).Kind = Kind_Number,
                 "expected two numbers");
      return (Kind_Number, Ada_Operator (Args (Args'First).Number,
                                         Args (Args'Last).Number));
   end Generic_Mal_Operator;

   generic
      with function Ada_Operator (Left, Right : in Integer) return Boolean;
   function Generic_Comparison (Args : in Types.T_Array) return Types.T;
   function Generic_Comparison (Args : in Types.T_Array) return Types.T is
   begin
      Err.Check (Args'Length = 2
                   and then Args (Args'First).Kind = Kind_Number
                   and then Args (Args'Last).Kind = Kind_Number,
                 "expected two numbers");
      return (Kind_Boolean, Ada_Operator (Args (Args'First).Number,
                                          Args (Args'Last).Number));
   end Generic_Comparison;

   function Addition      is new Generic_Mal_Operator ("+");
   function Apply         (Args : in Types.T_Array) return Types.T;
   function Division      is new Generic_Mal_Operator ("/");
   function Equals        (Args : in Types.T_Array) return Types.T;
   function Greater_Equal is new Generic_Comparison (">=");
   function Greater_Than  is new Generic_Comparison (">");
   function Is_Atom       is new Generic_Kind_Test (Kind_Atom);
   function Is_False      (Args : in Types.T_Array) return Types.T;
   function Is_Function   (Args : in Types.T_Array) return Types.T;
   function Is_Keyword    is new Generic_Kind_Test (Kind_Keyword);
   function Is_List       is new Generic_Kind_Test (Kind_List);
   function Is_Macro      is new Generic_Kind_Test (Kind_Macro);
   function Is_Map        is new Generic_Kind_Test (Kind_Map);
   function Is_Nil        is new Generic_Kind_Test (Kind_Nil);
   function Is_Number     is new Generic_Kind_Test (Kind_Number);
   function Is_Sequential (Args : in Types.T_Array) return Types.T;
   function Is_String     is new Generic_Kind_Test (Kind_String);
   function Is_Symbol     is new Generic_Kind_Test (Kind_Symbol);
   function Is_True       (Args : in Types.T_Array) return Types.T;
   function Is_Vector     is new Generic_Kind_Test (Kind_Vector);
   function Keyword       (Args : in Types.T_Array) return Types.T;
   function Less_Equal    is new Generic_Comparison ("<=");
   function Less_Than     is new Generic_Comparison ("<");
   function Meta          (Args : in Types.T_Array) return Types.T;
   function Pr_Str        (Args : in Types.T_Array) return Types.T;
   function Println       (Args : in Types.T_Array) return Types.T;
   function Prn           (Args : in Types.T_Array) return Types.T;
   function Product       is new Generic_Mal_Operator ("*");
   function Read_String   (Args : in Types.T_Array) return Types.T;
   function Readline      (Args : in Types.T_Array) return Types.T;
   function Seq           (Args : in Types.T_Array) return Types.T;
   function Slurp         (Args : in Types.T_Array) return Types.T;
   function Str           (Args : in Types.T_Array) return Types.T;
   function Subtraction   is new Generic_Mal_Operator ("-");
   function Symbol        (Args : in Types.T_Array) return Types.T;
   function Time_Ms       (Args : in Types.T_Array) return Types.T;
   function With_Meta     (Args : in Types.T_Array) return Types.T;

   ----------------------------------------------------------------------

   function Apply (Args : in Types.T_Array) return Types.T is
   begin
      Err.Check (2 <= Args'Length
                   and then Args (Args'Last).Kind in Types.Kind_Sequence,
                 "expected a function, optional arguments then a sequence");
      declare
         use type Types.T_Array;
         F : Types.T renames Args (Args'First);
         A : constant Types.T_Array
           := Args (Args'First + 1 .. Args'Last - 1)
           & Args (Args'Last).Sequence.all.Data;
      begin
         case F.Kind is
            when Kind_Builtin =>
               return F.Builtin.all (A);
            when Kind_Builtin_With_Meta =>
               return F.Builtin_With_Meta.all.Builtin.all (A);
            when Kind_Fn =>
               return F.Fn.all.Apply (A);
            when others =>
               Err.Raise_With ("parameter 1 must be a function");
         end case;
      end;
   end Apply;

   function Equals (Args : in Types.T_Array) return Types.T is
      use type Types.T;
   begin
      Err.Check (Args'Length = 2, "expected 2 parameters");
      return (Kind_Boolean, Args (Args'First) = Args (Args'Last));
   end Equals;

   function Is_False (Args : in Types.T_Array) return Types.T is
   begin
      Err.Check (Args'Length = 1, "expected 1 parameter");
      return (Kind_Boolean, Args (Args'First).Kind = Kind_Boolean
                and then not Args (Args'First).Ada_Boolean);
   end Is_False;

   function Is_Function (Args : in Types.T_Array) return Types.T is
   begin
      Err.Check (Args'Length = 1, "expected 1 parameter");
      return (Kind_Boolean, Args (Args'First).Kind in Types.Kind_Function);
   end Is_Function;

   function Is_Sequential (Args : in Types.T_Array) return Types.T is
   begin
      Err.Check (Args'Length = 1, "expected 1 parameter");
      return (Kind_Boolean, Args (Args'First).Kind in Types.Kind_Sequence);
   end Is_Sequential;

   function Is_True (Args : in Types.T_Array) return Types.T is
   begin
      Err.Check (Args'Length = 1, "expected 1 parameter");
      return (Kind_Boolean, Args (Args'First).Kind = Kind_Boolean
                and then Args (Args'First).Ada_Boolean);
   end Is_True;

   function Keyword (Args : in Types.T_Array) return Types.T is
   begin
      Err.Check (Args'Length = 1
                   and then Args (Args'First).Kind in Types.Kind_Key,
                 "expected a keyword or a string");
      return (Kind_Keyword, Args (Args'First).Str);
   end Keyword;

   function Meta (Args : in Types.T_Array) return Types.T is
   begin
      Err.Check (Args'Length = 1, "expected 1 parameter");
      declare
         A1 : Types.T renames Args (Args'First);
      begin
         case A1.Kind is
            when Types.Kind_Sequence =>
               return A1.Sequence.all.Meta;
            when Kind_Map =>
               return A1.Map.all.Meta;
            when Kind_Fn =>
               return A1.Fn.all.Meta;
            when Kind_Builtin_With_Meta =>
               return A1.Builtin_With_Meta.all.Meta;
            when Kind_Builtin =>
               return Types.Nil;
            when Kind_Atom =>
               return A1.Atom.all.Meta;
            when others =>
               Err.Raise_With ("expected an atom, function, map or sequence");
         end case;
      end;
   end Meta;

   procedure NS_Add_To_Repl (Repl : in Envs.Ptr) is
      procedure P (S : in String;
                   B : in Types.Builtin_Ptr) with Inline;
      procedure P (S : in String;
                   B : in Types.Builtin_Ptr)
      is
      begin
         Repl.all.Set ((Kind_Symbol, Types.Strings.Alloc (S)),
                       (Kind_Builtin, B));
      end P;
   begin
      P ("+",           Addition'Access);
      P ("apply",       Apply'Access);
      P ("assoc",       Types.Maps.Assoc'Access);
      P ("atom",        Types.Atoms.Atom'Access);
      P ("concat",      Types.Sequences.Concat'Access);
      P ("conj",        Types.Sequences.Conj'Access);
      P ("cons",        Types.Sequences.Cons'Access);
      P ("contains?",   Types.Maps.Contains'Access);
      P ("count",       Types.Sequences.Count'Access);
      P ("deref",       Types.Atoms.Deref'Access);
      P ("dissoc",      Types.Maps.Dissoc'Access);
      P ("/",           Division'Access);
      P ("=",           Equals'Access);
      P ("first",       Types.Sequences.First'Access);
      P ("get",         Types.Maps.Get'Access);
      P (">=",          Greater_Equal'Access);
      P (">",           Greater_Than'Access);
      P ("hash-map",    Types.Maps.Hash_Map'Access);
      P ("atom?",       Is_Atom'Access);
      P ("empty?",      Types.Sequences.Is_Empty'Access);
      P ("false?",      Is_False'Access);
      P ("fn?",         Is_Function'Access);
      P ("keyword?",    Is_Keyword'Access);
      P ("list?",       Is_List'Access);
      P ("macro?",      Is_Macro'Access);
      P ("map?",        Is_Map'Access);
      P ("nil?",        Is_Nil'Access);
      P ("number?",     Is_Number'Access);
      P ("sequential?", Is_Sequential'Access);
      P ("string?",     Is_String'Access);
      P ("symbol?",     Is_Symbol'Access);
      P ("true?",       Is_True'Access);
      P ("vector?",     Is_Vector'Access);
      P ("keys",        Types.Maps.Keys'Access);
      P ("keyword",     Keyword'Access);
      P ("<=",          Less_Equal'Access);
      P ("<",           Less_Than'Access);
      P ("list",        Types.Sequences.List'Access);
      P ("map",         Types.Sequences.Map'Access);
      P ("meta",        Meta'Access);
      P ("nth",         Types.Sequences.Nth'Access);
      P ("pr-str",      Pr_Str'Access);
      P ("println",     Println'Access);
      P ("prn",         Prn'Access);
      P ("*",           Product'Access);
      P ("read-string", Read_String'Access);
      P ("readline",    Readline'Access);
      P ("reset!",      Types.Atoms.Reset'Access);
      P ("rest",        Types.Sequences.Rest'Access);
      P ("seq",         Seq'Access);
      P ("slurp",       Slurp'Access);
      P ("str",         Str'Access);
      P ("-",           Subtraction'Access);
      P ("swap!",       Types.Atoms.Swap'Access);
      P ("symbol",      Symbol'Access);
      P ("throw",       Err.Throw'Access);
      P ("time-ms",     Time_Ms'Access);
      P ("vals",        Types.Maps.Vals'Access);
      P ("vec",         Types.Sequences.Vec'Access);
      P ("vector",      Types.Sequences.Vector'Access);
      P ("with-meta",   With_Meta'Access);
   end NS_Add_To_Repl;

   function Pr_Str (Args : in Types.T_Array) return Types.T is
      R       : ASU.Unbounded_String;
      Started : Boolean := False;
   begin
      for A of Args loop
         if Started then
            ASU.Append (R, ' ');
         else
            Started := True;
         end if;
         Printer.Pr_Str (R, A);
      end loop;
      return (Kind_String, Types.Strings.Alloc (ASU.To_String (R)));
   end Pr_Str;

   function Println (Args : in Types.T_Array) return Types.T is
      Started : Boolean := False;
      Buffer  : ASU.Unbounded_String;
   begin
      for A of Args loop
         if Started then
            ASU.Append (Buffer, ' ');
         else
            Started := True;
         end if;
         Printer.Pr_Str (Buffer, A, Readably => False);
      end loop;
      Ada.Text_IO.Unbounded_IO.Put_Line (Buffer);
      return Types.Nil;
   end Println;

   function Prn (Args : in Types.T_Array) return Types.T is
      --  Calling Pr_Str would create an intermediate copy.
      Buffer  : ASU.Unbounded_String;
      Started : Boolean := False;
   begin
      for A of Args loop
         if Started then
            ASU.Append (Buffer, ' ');
         else
            Started := True;
         end if;
         Printer.Pr_Str (Buffer, A);
      end loop;
      Ada.Text_IO.Unbounded_IO.Put_Line (Buffer);
      return Types.Nil;
   end Prn;

   function Readline (Args : in Types.T_Array) return Types.T is
   begin
      Err.Check (Args'Length = 1 and then Args (Args'First).Kind = Kind_String,
                 "expected a string");
      Ada.Text_IO.Put (Args (Args'First).Str.all.To_String);
      if Ada.Text_IO.End_Of_File then
         return Types.Nil;
      else
         return (Kind_String, Types.Strings.Alloc (Ada.Text_IO.Get_Line));
      end if;
   end Readline;

   function Read_String (Args : in Types.T_Array) return Types.T is
      Result : Types.T;
      procedure Process (Element : in String);
      procedure Process (Element : in String) is
         R : constant Types.T_Array := Reader.Read_Str (Element);
      begin
         Err.Check (R'Length = 1, "parameter must contain 1 expression");
         Result := R (R'First);
      end Process;
   begin
      Err.Check (Args'Length = 1 and then Args (Args'First).Kind = Kind_String,
                 "expected a string");
      Args (Args'First).Str.all.Query_Element (Process'Access);
      return Result;
   end Read_String;

   function Seq (Args : in Types.T_Array) return Types.T is
   begin
      Err.Check (Args'Length = 1, "expected 1 parameter");
      case Args (Args'First).Kind is
         when Kind_Nil =>
            return Types.Nil;
         when Kind_String =>
            declare
               Result : Types.T;
               procedure Process (S : in String);
               procedure Process (S : in String) is
               begin
                  if S'Length = 0 then
                     Result := Types.Nil;
                  else
                     Result := (Kind_List,
                                Types.Sequences.Constructor (S'Length));
                     for I in S'Range loop
                        Result.Sequence.all.Data (S'First - 1 + I)
                          := (Kind_String, Types.Strings.Alloc (S (I .. I)));
                     end loop;
                  end if;
               end Process;
            begin
               Args (Args'First).Str.all.Query_Element (Process'Access);
               return Result;
            end;
         when Types.Kind_Sequence =>
            if Args (Args'First).Sequence.all.Length = 0 then
               return Types.Nil;
            else
               return (Kind_List, Args (Args'First).Sequence);
            end if;
         when others =>
            Err.Raise_With ("expected nil, a sequence or a string");
      end case;
   end Seq;

   function Slurp (Args : in Types.T_Array) return Types.T is
      use Ada.Text_IO;
      File   : File_Type;
      Buffer : ASU.Unbounded_String;
   begin
      Err.Check (Args'Length = 1 and then Args (Args'First).Kind = Kind_String,
                 "expected a string");
      Open (File, In_File, Args (Args'First).Str.all.To_String);
      while not End_Of_File (File) loop
         ASU.Append (Buffer, Get_Line (File));
         ASU.Append (Buffer, Ada.Characters.Latin_1.LF);
      end loop;
      Close (File);
      return (Kind_String, Types.Strings.Alloc (ASU.To_String (Buffer)));
   exception
      --  Catch I/O errors, but not Err.Error...
      when E : Status_Error | Name_Error | Use_Error | Mode_Error =>
         if Is_Open (File) then
            Close (File);
         end if;
         Err.Raise_In_Mal (E);
   end Slurp;

   function Str (Args : in Types.T_Array) return Types.T is
      R : ASU.Unbounded_String;
   begin
      for Arg of Args loop
         Printer.Pr_Str (R, Arg, Readably => False);
      end loop;
      return (Kind_String, Types.Strings.Alloc (ASU.To_String (R)));
   end Str;

   function Symbol (Args : in Types.T_Array) return Types.T is
   begin
      Err.Check (Args'Length = 1 and then Args (Args'First).Kind = Kind_String,
                 "expected a string");
      return (Kind_Symbol, Args (Args'First).Str);
   end Symbol;

   function Time_Ms (Args : in Types.T_Array) return Types.T is
      use type Ada.Calendar.Time;
   begin
      Err.Check (Args'Length = 0, "expected no parameter");
      return (Kind_Number,
              Integer (1000.0 * (Ada.Calendar.Clock - Start_Time)));
   end Time_Ms;

   function With_Meta (Args : in Types.T_Array) return Types.T is
   begin
      Err.Check (Args'Length = 2, "expected 2 parameters");
      declare
         A1 : Types.T renames Args (Args'First);
         A2 : Types.T renames Args (Args'Last);
      begin
         case A1.Kind is
            when Kind_Builtin_With_Meta =>
               return A1.Builtin_With_Meta.all.With_Meta (A2);
            when Kind_Builtin =>
               return Types.Builtins.With_Meta (A1.Builtin, A2);
            when Kind_List =>
               return R : constant Types.T
                  := Types.Sequences.List (A1.Sequence.all.Data)
               do
                  R.Sequence.all.Meta := A2;
               end return;
            when Kind_Vector =>
               return R : constant Types.T
                  := Types.Sequences.Vector (A1.Sequence.all.Data)
               do
                  R.Sequence.all.Meta := A2;
               end return;
            when Kind_Map =>
               return A1.Map.all.With_Meta (A2);
            when Kind_Fn =>
               return (Kind_Fn, Types.Fns.New_Function
                         (A1.Fn.all.Params, A1.Fn.all.Ast, A1.Fn.all.Env, A2));
            when Kind_Atom =>
               return A1.Atom.all.With_Meta (A2);
            when others =>
               Err.Raise_With
                 ("parameter 1 must be a function, map or sequence");
         end case;
      end;
   end With_Meta;

end Core;
