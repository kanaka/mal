with Ada.Calendar;
with Ada.Characters.Latin_1;
with Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO;

with Envs;
with Eval_Cb;
with Types.Atoms;
with Types.Builtins;
with Types.Functions;
with Types.Lists;
with Types.Maps;
with Types.Symbols.Names;
with Printer;
with Reader;

package body Core is

   use Types;
   package ASU renames Ada.Strings.Unbounded;

   --  Used by time_ms.
   Start_Time : constant Ada.Calendar.Time := Ada.Calendar.Clock;

   --  In the following helpers, "name" is the one reported by error
   --  messages.

   generic
      Kind : in Kind_Type;
      Name : in String;
   function Generic_Kind_Test (Args : in Mal.T_Array) return Mal.T;
   function Generic_Kind_Test (Args : in Mal.T_Array) return Mal.T
   is (if Args'Length /= 1 then
          raise Argument_Error with Name & ": expects 1 argument"
       else
          (Kind_Boolean, Args (Args'First).Kind = Kind));

   generic
      with function Ada_Operator (Left, Right : in Integer) return Integer;
      Name : in String;
   function Generic_Mal_Operator (Args : in Mal.T_Array) return Mal.T;
   function Generic_Mal_Operator (Args : in Mal.T_Array) return Mal.T
   is (if Args'Length /= 2 then
          raise Argument_Error with Name & ": expects 2 arguments"
       elsif (for some A of Args => A.Kind /= Kind_Number) then
          raise Argument_Error with Name & ": expects numbers"
       else
          (Kind_Number, Ada_Operator (Args (Args'First).Number,
                                      Args (Args'Last).Number)));
   generic
      with function Ada_Operator (Left, Right : in Integer) return Boolean;
      Name : in String;
   function Generic_Comparison (Args : in Mal.T_Array) return Mal.T;
   function Generic_Comparison (Args : in Mal.T_Array) return Mal.T
   is (if Args'Length /= 2 then
          raise Argument_Error with Name & ": expects 2 arguments"
       elsif (for some A of Args => A.Kind /= Kind_Number) then
          raise Argument_Error with Name & ": expects numbers"
       else
          (Kind_Boolean, Ada_Operator (Args (Args'First).Number,
                                       Args (Args'Last).Number)));

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
   function Symbol        (Args : in Mal.T_Array) return Mal.T;
   function Throw         (Args : in Mal.T_Array) return Mal.T;
   function Time_Ms       (Args : in Mal.T_Array) return Mal.T;
   function With_Meta     (Args : in Mal.T_Array) return Mal.T;

   ----------------------------------------------------------------------

   function Apply (Args : in Mal.T_Array) return Mal.T is
      use type Lists.Ptr;
   begin
      if Args'Length < 2 then
         raise Argument_Error with "apply: expects at least 2 arguments";
      elsif Args (Args'Last).Kind not in Kind_List | Kind_Vector then
         raise Argument_Error with "apply: last arg must be a list or vector";
      end if;
      declare
         F : Mal.T renames Args (Args'First);
         A : constant Mal.T_Array
           := Args (Args'First + 1 .. Args'Last - 1) & Args (Args'Last).List;
      begin
         case F.Kind is
            when Kind_Builtin =>
               return F.Builtin.all (A);
            when Kind_Builtin_With_Meta =>
               return F.Builtin_With_Meta.Builtin.all (A);
            when Kind_Function =>
               return F.Fn.Apply (A);
            when others =>
               raise Argument_Error
                 with "apply: cannot call " & Printer.Img (F);
         end case;
      end;
   end Apply;

   function Equals (Args : in Mal.T_Array) return Mal.T is
      use type Mal.T;
   begin
      if Args'Length /= 2 then
         raise Argument_Error with "=: expects 2 arguments";
      else
         return (Kind_Boolean, Args (Args'First) = Args (Args'Last));
      end if;
   end Equals;

   function Eval (Args : in Mal.T_Array) return Mal.T
   is (if Args'Length /= 1 then
          raise Argument_Error with "eval: expects 1 argument"
       else
          Eval_Cb.Cb.all (Ast => Args (Args'First),
                          Env => Envs.Repl));

   function Is_False (Args : in Mal.T_Array) return Mal.T
   is (if Args'Length /= 1 then
          raise Argument_Error with "false?: expects 1 argument"
       else
          (Kind_Boolean, Args (Args'First).Kind = Kind_Boolean
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

   function Meta (Args : in Mal.T_Array) return Mal.T is
   begin
      if Args'Length /= 1 then
         raise Argument_Error with "meta: expects 1 argument";
      end if;
      declare
         A1 : Mal.T renames Args (Args'First);
      begin
         case A1.Kind is
            when Kind_List | Kind_Vector =>
               return A1.List.Meta;
            when Kind_Map =>
               return A1.Map.Meta;
            when Kind_Function =>
               return A1.Fn.Meta;
            when Kind_Builtin_With_Meta =>
               return A1.Builtin_With_Meta.Meta;
            when Kind_Builtin =>
               return Mal.Nil;
            when others =>
               raise Argument_Error
                 with "meta: expects a list, vector, map or function";
         end case;
      end;
   end Meta;

   function Ns return Binding_List
   is ((Symbols.Constructor ("+"),           Addition'Access),
       (Symbols.Constructor ("apply"),       Apply'Access),
       (Symbols.Constructor ("assoc"),       Maps.Assoc'Access),
       (Symbols.Constructor ("atom"),        Atoms.Atom'Access),
       (Symbols.Constructor ("concat"),      Lists.Concat'Access),
       (Symbols.Constructor ("conj"),        Lists.Conj'Access),
       (Symbols.Constructor ("cons"),        Lists.Cons'Access),
       (Symbols.Constructor ("contains?"),   Maps.Contains'Access),
       (Symbols.Constructor ("count"),       Lists.Count'Access),
       (Symbols.Names.Deref,                 Atoms.Deref'Access),
       (Symbols.Constructor ("dissoc"),      Maps.Dissoc'Access),
       (Symbols.Constructor ("/"),           Division'Access),
       (Symbols.Constructor ("="),           Equals'Access),
       (Symbols.Constructor ("eval"),        Eval'Access),
       (Symbols.Constructor ("first"),       Lists.First'Access),
       (Symbols.Constructor ("get"),         Maps.Get'Access),
       (Symbols.Constructor (">="),          Greater_Equal'Access),
       (Symbols.Constructor (">"),           Greater_Than'Access),
       (Symbols.Constructor ("hash-map"),    Maps.Hash_Map'Access),
       (Symbols.Constructor ("atom?"),       Is_Atom'Access),
       (Symbols.Constructor ("empty?"),      Lists.Is_Empty'Access),
       (Symbols.Constructor ("false?"),      Is_False'Access),
       (Symbols.Constructor ("fn?"),         Is_Function'Access),
       (Symbols.Constructor ("keyword?"),    Is_Keyword'Access),
       (Symbols.Constructor ("list?"),       Is_List'Access),
       (Symbols.Constructor ("macro?"),      Is_Macro'Access),
       (Symbols.Constructor ("map?"),        Is_Map'Access),
       (Symbols.Constructor ("nil?"),        Is_Nil'Access),
       (Symbols.Constructor ("number?"),     Is_Number'Access),
       (Symbols.Constructor ("sequential?"), Is_Sequential'Access),
       (Symbols.Constructor ("string?"),     Is_String'Access),
       (Symbols.Constructor ("symbol?"),     Is_Symbol'Access),
       (Symbols.Constructor ("true?"),       Is_True'Access),
       (Symbols.Constructor ("vector?"),     Is_Vector'Access),
       (Symbols.Constructor ("keys"),        Maps.Keys'Access),
       (Symbols.Constructor ("keyword"),     Keyword'Access),
       (Symbols.Constructor ("<="),          Less_Equal'Access),
       (Symbols.Constructor ("<"),           Less_Than'Access),
       (Symbols.Constructor ("list"),        Lists.List'Access),
       (Symbols.Constructor ("map"),         Lists.Map'Access),
       (Symbols.Constructor ("meta"),        Meta'Access),
       (Symbols.Constructor ("nth"),         Lists.Nth'Access),
       (Symbols.Constructor ("pr-str"),      Pr_Str'Access),
       (Symbols.Constructor ("println"),     Println'Access),
       (Symbols.Constructor ("prn"),         Prn'Access),
       (Symbols.Constructor ("*"),           Product'Access),
       (Symbols.Constructor ("read-string"), Read_String'Access),
       (Symbols.Constructor ("readline"),    Readline'Access),
       (Symbols.Constructor ("reset!"),      Atoms.Reset'Access),
       (Symbols.Constructor ("rest"),        Lists.Rest'Access),
       (Symbols.Constructor ("seq"),         Seq'Access),
       (Symbols.Constructor ("slurp"),       Slurp'Access),
       (Symbols.Constructor ("str"),         Str'Access),
       (Symbols.Constructor ("-"),           Subtraction'Access),
       (Symbols.Constructor ("swap!"),       Atoms.Swap'Access),
       (Symbols.Constructor ("symbol"),      Symbol'Access),
       (Symbols.Constructor ("throw"),       Throw'Access),
       (Symbols.Constructor ("time-ms"),     Time_Ms'Access),
       (Symbols.Constructor ("vals"),        Maps.Vals'Access),
       (Symbols.Constructor ("vector"),      Lists.Vector'Access),
       (Symbols.Names.With_Meta,             With_Meta'Access));

   function Pr_Str (Args : in Mal.T_Array) return Mal.T is
      R       : ASU.Unbounded_String := ASU.Null_Unbounded_String;
      Started : Boolean := False;
   begin
      for A of Args loop
         if Started then
            ASU.Append (R, ' ');
         else
            Started := True;
         end if;
         ASU.Append (R, Printer.Pr_Str (A));
      end loop;
      return (Kind_String, R);
   end Pr_Str;

   function Println (Args : in Mal.T_Array) return Mal.T is
      Started : Boolean := False;
   begin
      for A of Args loop
         if Started then
            Ada.Text_IO.Put (' ');
         else
            Started := True;
         end if;
         Ada.Text_IO.Unbounded_IO.Put (Printer.Pr_Str (A, Readably => False));
      end loop;
      Ada.Text_IO.New_Line;
      return Mal.Nil;
   end Println;

   function Prn (Args : in Mal.T_Array) return Mal.T is
      Started : Boolean := False;
   begin
      for A of Args loop
         if Started then
            Ada.Text_IO.Put (' ');
         else
            Started := True;
         end if;
         Ada.Text_IO.Unbounded_IO.Put (Printer.Pr_Str (A));
      end loop;
      Ada.Text_IO.New_Line;
      return Mal.Nil;
   end Prn;

   function Readline (Args : in Mal.T_Array) return Mal.T is
   begin
      if Args'Length /= 1 then
         raise Argument_Error with "readline: expects 1 argument";
      elsif Args (Args'First).Kind not in Kind_Keyword | Kind_String then
         raise Argument_Error with "readline: expects a keyword or string";
      end if;
      Ada.Text_IO.Unbounded_IO.Put (Args (Args'First).S);
      if Ada.Text_IO.End_Of_File then
         return Mal.Nil;
      else
         return (Kind_String, Ada.Text_IO.Unbounded_IO.Get_Line);
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
            if Args (Args'First).List.Length = 0 then
               return Mal.Nil;
            else
               return (Kind_List, Args (Args'First).List);
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
      R : ASU.Unbounded_String := ASU.Null_Unbounded_String;
   begin
      for A of Args loop
         ASU.Append (R, Printer.Pr_Str (A, Readably => False));
      end loop;
      return (Kind_String, R);
   end Str;

   function Symbol (Args : in Mal.T_Array) return Mal.T
   is (if Args'Length /= 1 then
          raise Argument_Error with "symbol: expects 1 argument"
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

   function With_Meta (Args : in Mal.T_Array) return Mal.T is
   begin
      if Args'Length /= 2 then
         raise Argument_Error with "with-meta: expects 2 arguments";
      end if;
      declare
         A1 : Mal.T renames Args (Args'First);
         A2 : Mal.T renames Args (Args'Last);
      begin
         case A1.Kind is
            when Kind_Builtin_With_Meta =>
               return A1.Builtin_With_Meta.With_Meta (A2);
            when Kind_Builtin =>
               return Builtins.With_Meta (A1.Builtin, A2);
            when Kind_List =>
               return (Kind_List, A1.List.With_Meta (A2));
            when Kind_Vector =>
               return (Kind_Vector, A1.List.With_Meta (A2));
            when Kind_Map =>
               return A1.Map.With_Meta (A2);
            when Kind_Function =>
               return A1.Fn.With_Meta (A2);
            when others =>
               raise Argument_Error
                 with "with-meta: expects a list, vector, map or function";
         end case;
      end;
   end With_Meta;

end Core;
