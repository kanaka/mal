with Ada.Calendar;
with Ada.Characters.Latin_1;
with Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO;

with Environments; pragma Elaborate_All (Environments);
with Types.Atoms;
with Types.Builtins;
with Types.Functions;
with Types.Lists;
with Types.Maps;
with Types.Symbols.Names; pragma Elaborate_All (Types.Symbols);
with Printer;
with Reader;

package body Core is

   use Types;
   use type Mal.T;

   package ASU renames Ada.Strings.Unbounded;

   Start_Time : constant Ada.Calendar.Time := Ada.Calendar.Clock;

   function Apply_Helper (Func : in Mal.T;
                          Args : in Mal.T_Array;
                          Name : in String) return Mal.T with Inline;
   --  If Func is not executable, report an exception using "name" as
   --  the built-in function name.

   generic
      Kind : in Kind_Type;
      Name : in String;
   function Generic_Kind_Test (Args : in Mal.T_Array) return Mal.T;
   function Generic_Kind_Test (Args : in Mal.T_Array) return Mal.T
   is (if Args'Length /= 1
       then raise Argument_Error with Name & ": expects 1 argument"
       else (Kind_Boolean, Args (Args'First).Kind = Kind));

   generic
      with function Ada_Operator (Left, Right : in Integer) return Integer;
      Name : in String;
   function Generic_Mal_Operator (Args : in Mal.T_Array) return Mal.T;
   function Generic_Mal_Operator (Args : in Mal.T_Array) return Mal.T
   is (if Args'Length /= 2
       then raise Argument_Error with Name & ": expects 2 arguments"
       elsif (for some A of Args => A.Kind /= Kind_Number)
       then raise Argument_Error with Name & ": expects numbers"
       else (Kind_Number, Ada_Operator (Args (Args'First).Ada_Number,
                                        Args (Args'Last).Ada_Number)));

   generic
      with function Ada_Operator (Left, Right : in Integer) return Boolean;
      Name : in String;
   function Generic_Comparison (Args : in Mal.T_Array) return Mal.T;
   function Generic_Comparison (Args : in Mal.T_Array) return Mal.T
   is (if Args'Length /= 2
       then raise Argument_Error with Name & ": expects 2 arguments"
       elsif (for some A of Args => A.Kind /= Kind_Number)
       then raise Argument_Error with Name & ": expects numbers"
       else (Kind_Boolean, Ada_Operator (Args (Args'First).Ada_Number,
                                         Args (Args'Last).Ada_Number)));

   --  Built-in functions from this package.
   function Addition      is new Generic_Mal_Operator ("+", "+");
   function Apply         (Args : in Mal.T_Array) return Mal.T;
   function Division      is new Generic_Mal_Operator ("/", "/");
   function Equals        (Args : in Mal.T_Array) return Mal.T;
   function Eval          (Args : in Mal.T_Array) return Mal.T;
   function Greater_Equal is new Generic_Comparison (">=", ">=");
   function Greater_Than  is new Generic_Comparison (">", ">");
   function Is_Atom       is new Generic_Kind_Test (Kind_Atom, "atom?");
   function Is_False      (Args : in Mal.T_Array) return Mal.T;
   function Is_Function   (Args : in Mal.T_Array) return Mal.T;
   function Is_Keyword    is new Generic_Kind_Test (Kind_Keyword, "keyword?");
   function Is_List       is new Generic_Kind_Test (Kind_List, "list?");
   function Is_Macro      is new Generic_Kind_Test (Kind_Macro, "macro?");
   function Is_Map        is new Generic_Kind_Test (Kind_Map, "map?");
   function Is_Nil        is new Generic_Kind_Test (Kind_Nil, "nil?");
   function Is_Number     is new Generic_Kind_Test (Kind_Number, "number?");
   function Is_Sequential (Args : in Mal.T_Array) return Mal.T;
   function Is_String     is new Generic_Kind_Test (Kind_String, "string?");
   function Is_Symbol     is new Generic_Kind_Test (Kind_Symbol, "symbol?");
   function Is_True       (Args : in Mal.T_Array) return Mal.T;
   function Is_Vector     is new Generic_Kind_Test (Kind_Vector, "vector?");
   function Keyword       (Args : in Mal.T_Array) return Mal.T;
   function Less_Equal    is new Generic_Comparison ("<=", "<=");
   function Less_Than     is new Generic_Comparison ("<", "<");
   function Map           (Args : in Mal.T_Array) return Mal.T;
   function Meta          (Args : in Mal.T_Array) return Mal.T;
   function Pr_Str        (Args : in Mal.T_Array) return Mal.T;
   function Println       (Args : in Mal.T_Array) return Mal.T;
   function Prn           (Args : in Mal.T_Array) return Mal.T;
   function Product       is new Generic_Mal_Operator ("*", "*");
   function Read_String   (Args : in Mal.T_Array) return Mal.T;
   function Readline      (Args : in Mal.T_Array) return Mal.T;
   function Seq           (Args : in Mal.T_Array) return Mal.T;
   function Slurp         (Args : in Mal.T_Array) return Mal.T;
   function Str           (Args : in Mal.T_Array) return Mal.T;
   function Subtraction   is new Generic_Mal_Operator ("-", "-");
   function Swap          (Args : in Mal.T_Array) return Mal.T;
   function Symbol        (Args : in Mal.T_Array) return Mal.T;
   function Throw         (Args : in Mal.T_Array) return Mal.T;
   function Time_Ms       (Args : in Mal.T_Array) return Mal.T;
   function With_Meta     (Args : in Mal.T_Array) return Mal.T;

   ----------------------------------------------------------------------

   function Apply_Helper (Func : in Mal.T;
                          Args : in Mal.T_Array;
                          Name : in String) return Mal.T
   is
   begin
      case Func.Kind is
      when Kind_Builtin =>
         return Func.Builtin.all (Args);
      when Kind_Builtin_With_Meta =>
         return Func.Builtin_With_Meta.Data.all (Args);
      when Kind_Function =>
         declare
            Env : constant Environments.Ptr
              := Func.Function_Value.Closure.Closure_Sub;
         begin
            Func.Function_Value.Set_Binds (Env, Args);
            return Eval_Ref.all (Func.Function_Value.Expression, Env);
         end;
      when Kind_Nil | Kind_Atom | Kind_Boolean | Kind_Number | Kind_String
        | Kind_Symbol | Kind_Keyword | Kind_List | Kind_Vector | Kind_Map
        | Kind_Macro =>
         raise Argument_Error with Name & ": cannot execute "
           & ASU.To_String (Printer.Pr_Str (Func));
      end case;
   end Apply_Helper;

   function Apply (Args : in Mal.T_Array) return Mal.T is
      use type Lists.Ptr;
   begin
      if Args'Length < 2 then
         raise Argument_Error with "apply: expects at least 2 arguments";
      elsif Args (Args'Last).Kind not in Kind_List | Kind_Vector then
         raise Argument_Error with "apply: last arg must a be list or vector";
      else
         return Apply_Helper (Args (Args'First),
                              Args (Args'First + 1 .. Args'Last - 1)
                                & Args (Args'Last).L,
                              "apply");
      end if;
   end Apply;

   function Equals (Args : in Mal.T_Array) return Mal.T
   is (if Args'Length /= 2 then
          raise Argument_Error with "=: expects 2 arguments"
       else
          (Kind_Boolean, Args (Args'First) = Args (Args'Last)));

   function Eval (Args : in Mal.T_Array) return Mal.T
   is (if Args'Length /= 1 then
          raise Argument_Error with "eval: expects 1 argument"
       else
          Eval_Ref.all (Args (Args'First), Environments.Repl));

   function Is_False (Args : in Mal.T_Array) return Mal.T
   is (if Args'Length /= 1 then
          raise Argument_Error with "false?: expects 1 argument"
       else (Kind_Boolean, Args (Args'First).Kind = Kind_Boolean
                           and then not Args (Args'First).Ada_Boolean));

   function Is_Function (Args : in Mal.T_Array) return Mal.T
   is (if Args'Length /= 1 then
          raise Argument_Error with "count: expects 1 argument"
       else
          (Kind_Boolean, Args (Args'First).Kind in
             Kind_Function | Kind_Builtin | Kind_Builtin_With_Meta));

   function Is_Sequential (Args : in Mal.T_Array) return Mal.T
   is (if Args'Length /= 1 then
          raise Argument_Error with "sequential?: expects 1 argument"
       else
          (Kind_Boolean, Args (Args'First).Kind in Kind_List | Kind_Vector));

   function Is_True (Args : in Mal.T_Array) return Mal.T
   is (if Args'Length /= 1 then
          raise Argument_Error with "true?: expects 1 argument"
       else
          (Kind_Boolean, Args (Args'First).Kind = Kind_Boolean
                         and then Args (Args'First).Ada_Boolean));

   function Keyword (Args : in Mal.T_Array) return Mal.T
   is (if Args'Length /= 1 then
          raise Argument_Error with "keyword: expects 1 argument"
       elsif Args (Args'First).Kind not in Kind_Keyword | Kind_String then
          raise Argument_Error with "keyword: expects a keyword or a string"
       else
          (Kind_Keyword, Args (Args'First).S));

   function Map (Args : in Mal.T_Array) return Mal.T is
   begin
      if Args'Length /= 2 then
         raise Argument_Error with "map: expects 2 arguments";
      elsif Args (Args'Last).Kind not in Kind_List | Kind_Vector then
         raise Argument_Error with "map: arg  2 must be a list or vector";
      end if;
      declare
         R  : Mal.T_Array (1 .. Args (Args'Last).L.Length);
      begin
         for I in R'Range loop
            R (I) := Apply_Helper (Args (Args'First),
                        Mal.T_Array'(1 => Args (Args'Last).L.Element (I)),
                                   "map");
         end loop;
         return Lists.List (R);
      end;
   end Map;

   function Meta (Args : in Mal.T_Array) return Mal.T
   is (if Args'Length /= 1 then
          raise Argument_Error with "meta: expects 1 argument"
       else
         (case Args (Args'First).Kind is
            when Kind_List | Kind_Vector =>
               Args (Args'First).L.Meta,
            when Kind_Map =>
               Args (Args'First).Map.Meta,
            when Kind_Function =>
               Args (Args'First).Function_Value.Meta,
            when Kind_Builtin_With_Meta =>
               Args (Args'First).Builtin_With_Meta.Meta,
            when Kind_Builtin =>
                Mal.Nil,
            when Kind_Nil | Kind_Atom | Kind_Boolean | Kind_Number |
               Kind_String | Kind_Symbol | Kind_Keyword | Kind_Macro =>
               raise Argument_Error
                 with "meta: expects a list, vector, map or function"));

   function Pr_Str (Args : in Mal.T_Array) return Mal.T is
   begin
      return R : Mal.T := (Kind_String, ASU.Null_Unbounded_String) do
         if 0 < Args'Length then
            ASU.Append (R.S, Printer.Pr_Str (Args (Args'First)));
            for I in Args'First + 1 .. Args'Last loop
               ASU.Append (R.S, ' ');
               ASU.Append (R.S, Printer.Pr_Str (Args (I)));
            end loop;
         end if;
      end return;
   end Pr_Str;

   function Println (Args : in Mal.T_Array) return Mal.T is
      use Ada.Text_IO.Unbounded_IO;
   begin
      if 0 < Args'Length then
         Put (Printer.Pr_Str (Args (Args'First), Readably => False));
         for I in Args'First + 1 .. Args'Last loop
            Ada.Text_IO.Put (' ');
            Put (Printer.Pr_Str (Args (I), Readably => False));
         end loop;
      end if;
      Ada.Text_IO.New_Line;
      return Mal.Nil;
   end Println;

   function Prn (Args : in Mal.T_Array) return Mal.T is
   begin
      if 0 < Args'Length then
         Ada.Text_IO.Unbounded_IO.Put (Printer.Pr_Str (Args (Args'First)));
         for I in Args'First + 1 .. Args'Last loop
            Ada.Text_IO.Put (' ');
            Ada.Text_IO.Unbounded_IO.Put (Printer.Pr_Str (Args (I)));
         end loop;
      end if;
      Ada.Text_IO.New_Line;
      return Mal.Nil;
   end Prn;

   function Readline (Args : in Mal.T_Array) return Mal.T is
   begin
      if Args'Length /= 1 then
         raise Argument_Error with "readline: expects 1 argument";
      elsif Args (Args'First).Kind not in Kind_Keyword | Kind_String then
         raise Argument_Error with "readline: expects a keyword or string";
      else
         Ada.Text_IO.Unbounded_IO.Put (Args (Args'First).S);
         if Ada.Text_IO.End_Of_File then
            return Mal.Nil;
         else
            return (Kind_String, Ada.Text_IO.Unbounded_IO.Get_Line);
         end if;
      end if;
   end Readline;

   function Read_String (Args : in Mal.T_Array) return Mal.T
   is (if Args'Length /= 1 then
          raise Argument_Error with "read-string: expects 1 argument"
       elsif Args (Args'First).Kind /= Kind_String then
          raise Argument_Error with "read-string: expects a string"
       else
          Reader.Read_Str (ASU.To_String (Args (Args'First).S)));

   function Seq (Args : in Mal.T_Array) return Mal.T is
   begin
      if Args'Length /= 1 then
         raise Argument_Error with "seq: expects 1 argument";
      end if;
      case Args (Args'First).Kind is
         when Kind_Nil =>
            return Mal.Nil;
         when Kind_String =>
            if ASU.Length (Args (Args'First).S) = 0 then
               return Mal.Nil;
            else
               declare
                  A1 : constant ASU.Unbounded_String := Args (Args'First).S;
                  R  : Mal.T_Array (1 .. ASU.Length (A1));
               begin
                  for I in R'Range loop
                     R (I) := (Kind_String, ASU.Unbounded_Slice (A1, I, I));
                  end loop;
                  return Lists.List (R);
               end;
            end if;
         when Kind_List | Kind_Vector =>
            if Args (Args'First).L.Length = 0 then
               return Mal.Nil;
            else
               return (Kind_List, Args (Args'First).L);
            end if;
         when others =>
            raise Argument_Error with "seq: expects a string, list or vector";
      end case;
   end Seq;

   function Slurp (Args : in Mal.T_Array) return Mal.T is
      use Ada.Text_IO;
      File   : File_Type;
      Buffer : ASU.Unbounded_String;
   begin
      if Args'Length /= 1 then
         raise Argument_Error with "slurp: expects 1 argument";
      elsif Args (Args'First).Kind /= Kind_String then
         raise Argument_Error with "slurp: expects a string";
      else
         Open (File, In_File, ASU.To_String (Args (Args'First).S));
         while not End_Of_File (File) loop
            ASU.Append (Buffer, Get_Line (File));
            ASU.Append (Buffer, Ada.Characters.Latin_1.LF);
         end loop;
         Close (File);
         return (Kind_String, Buffer);
      end if;
   exception
      when others =>
         Close (File);
         raise;
   end Slurp;

   function Str (Args : in Mal.T_Array) return Mal.T is
   begin
      return R : Mal.T := (Kind_String, ASU.Null_Unbounded_String) do
         for Arg of Args loop
            ASU.Append (R.S, Printer.Pr_Str (Arg, Readably => False));
         end loop;
      end return;
   end Str;

   function Swap (Args : in Mal.T_Array) return Mal.T is
   begin
      if Args'Length < 2 then
         raise Argument_Error with "swap!: expects at least 2 arguments";
      elsif Args (Args'First).Kind /= Kind_Atom then
         raise Argument_Error with "swap!: arg 1 must be an atom";
      end if;
      declare
         use type Mal.T_Array;
         X  : Mal.T renames Atoms.Deref (Args (Args'First .. Args'First));
         FX : Mal.T renames Apply_Helper (Args (Args'First + 1),
                               X & Args (Args'First + 2 .. Args'Last),
                                          "swap!");
      begin
         return Atoms.Reset (Mal.T_Array'(Args (Args'First), FX));
      end;
   end Swap;

   function Symbol (Args : in Mal.T_Array) return Mal.T
   is (if Args'Length /= 1 then
          raise Argument_Error with "symbol?: expects 1 argument"
       else
          (Kind_Symbol,
           Symbols.Constructor (ASU.To_String (Args (Args'First).S))));

   function Throw (Args : in Mal.T_Array) return Mal.T is
   begin
      if Args'Length /= 1 then
         raise Argument_Error with "throw: expects 1 argument";
      end if;
      Last_Exception := Args (Args'First);
      raise Exception_Throwed;
      return Mal.Nil;                --  GNAT wants a return
   end Throw;

   function Time_Ms (Args : in Mal.T_Array) return Mal.T is
      use type Ada.Calendar.Time;
   begin
      if 0 < Args'Length then
         raise Argument_Error with "time: expects no argument";
      end if;
      return (Kind_Number,
              Integer (1000.0 * (Ada.Calendar.Clock - Start_Time)));
   end Time_Ms;

   function With_Meta (Args : in Mal.T_Array) return Mal.T
   is (if Args'Length /= 2 then
          raise Argument_Error with "with-meta: expects 2 arguments"
       else (case Args (Args'First).Kind is
          when Kind_Builtin_With_Meta =>
             Args (Args'First).Builtin_With_Meta.With_Meta (Args (Args'Last)),
          when Kind_Builtin =>
             Builtins.With_Meta (Args (Args'First).Builtin, Args (Args'Last)),
          when Kind_List =>
             (Kind_List, Args (Args'First).L.With_Meta (Args (Args'Last))),
          when Kind_Vector =>
             (Kind_Vector, Args (Args'First).L.With_Meta (Args (Args'Last))),
          when Kind_Map =>
             Args (Args'First).Map.With_Meta (Args (Args'Last)),
          when Kind_Function =>
             Args (Args'First).Function_Value.With_Meta (Args (Args'Last)),
          when others =>
             Args (Args'First)));

   use Symbols;
   R : Environments.Ptr renames Environments.Repl;
   B : Kind_Type renames Kind_Builtin;
begin                                   --  Core
   R.Set (Constructor ("+"),           (B, Addition'Access));
   R.Set (Constructor ("apply"),       (B, Apply'Access));
   R.Set (Constructor ("assoc"),       (B, Maps.Assoc'Access));
   R.Set (Constructor ("atom"),        (B, Atoms.Atom'Access));
   R.Set (Constructor ("concat"),      (B, Lists.Concat'Access));
   R.Set (Constructor ("conj"),        (B, Lists.Conj'Access));
   R.Set (Constructor ("cons"),        (B, Lists.Cons'Access));
   R.Set (Constructor ("contains?"),   (B, Maps.Contains'Access));
   R.Set (Constructor ("count"),       (B, Lists.Count'Access));
   R.Set (Names.Deref,                 (B, Atoms.Deref'Access));
   R.Set (Constructor ("dissoc"),      (B, Maps.Dissoc'Access));
   R.Set (Constructor ("/"),           (B, Division'Access));
   R.Set (Constructor ("="),           (B, Equals'Access));
   R.Set (Constructor ("eval"),        (B, Eval'Access));
   R.Set (Constructor ("first"),       (B, Lists.First'Access));
   R.Set (Constructor ("get"),         (B, Maps.Get'Access));
   R.Set (Constructor (">="),          (B, Greater_Equal'Access));
   R.Set (Constructor (">"),           (B, Greater_Than'Access));
   R.Set (Constructor ("hash-map"),    (B, Maps.Hash_Map'Access));
   R.Set (Constructor ("atom?"),       (B, Is_Atom'Access));
   R.Set (Constructor ("empty?"),      (B, Lists.Is_Empty'Access));
   R.Set (Constructor ("false?"),      (B, Is_False'Access));
   R.Set (Constructor ("fn?"),         (B, Is_Function'Access));
   R.Set (Constructor ("keyword?"),    (B, Is_Keyword'Access));
   R.Set (Constructor ("list?"),       (B, Is_List'Access));
   R.Set (Constructor ("macro?"),      (B, Is_Macro'Access));
   R.Set (Constructor ("map?"),        (B, Is_Map'Access));
   R.Set (Constructor ("nil?"),        (B, Is_Nil'Access));
   R.Set (Constructor ("number?"),     (B, Is_Number'Access));
   R.Set (Constructor ("sequential?"), (B, Is_Sequential'Access));
   R.Set (Constructor ("string?"),     (B, Is_String'Access));
   R.Set (Constructor ("symbol?"),     (B, Is_Symbol'Access));
   R.Set (Constructor ("true?"),       (B, Is_True'Access));
   R.Set (Constructor ("vector?"),     (B, Is_Vector'Access));
   R.Set (Constructor ("keys"),        (B, Maps.Keys'Access));
   R.Set (Constructor ("keyword"),     (B, Keyword'Access));
   R.Set (Constructor ("<="),          (B, Less_Equal'Access));
   R.Set (Constructor ("<"),           (B, Less_Than'Access));
   R.Set (Constructor ("list"),        (B, Lists.List'Access));
   R.Set (Constructor ("map"),         (B, Map'Access));
   R.Set (Constructor ("meta"),        (B, Meta'Access));
   R.Set (Constructor ("nth"),         (B, Lists.Nth'Access));
   R.Set (Constructor ("pr-str"),      (B, Pr_Str'Access));
   R.Set (Constructor ("println"),     (B, Println'Access));
   R.Set (Constructor ("prn"),         (B, Prn'Access));
   R.Set (Constructor ("*"),           (B, Product'Access));
   R.Set (Constructor ("read-string"), (B, Read_String'Access));
   R.Set (Constructor ("readline"),    (B, Readline'Access));
   R.Set (Constructor ("reset!"),      (B, Atoms.Reset'Access));
   R.Set (Constructor ("rest"),        (B, Lists.Rest'Access));
   R.Set (Constructor ("seq"),         (B, Seq'Access));
   R.Set (Constructor ("slurp"),       (B, Slurp'Access));
   R.Set (Constructor ("str"),         (B, Str'Access));
   R.Set (Constructor ("-"),           (B, Subtraction'Access));
   R.Set (Constructor ("swap!"),       (B, Swap'Access));
   R.Set (Constructor ("symbol"),      (B, Symbol'Access));
   R.Set (Constructor ("throw"),       (B, Throw'Access));
   R.Set (Constructor ("time-ms"),     (B, Time_Ms'Access));
   R.Set (Constructor ("vals"),        (B, Maps.Vals'Access));
   R.Set (Constructor ("vector"),      (B, Lists.Vector'Access));
   R.Set (Names.With_Meta,             (B, With_Meta'Access));
end Core;
