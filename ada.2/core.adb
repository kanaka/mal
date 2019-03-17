with Ada.Calendar;
with Ada.Characters.Latin_1;
with Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO;

with Envs;
with Err;
with Eval_Cb;
with Types.Atoms;
with Types.Builtins;
with Types.Mal;
with Types.Maps;
with Types.Sequences;
with Types.Symbols.Names;
with Printer;
with Reader;

package body Core is

   use Types;
   package ASU renames Ada.Strings.Unbounded;

   --  Used by time_ms.
   Start_Time : constant Ada.Calendar.Time := Ada.Calendar.Clock;

   generic
      Kind : in Kind_Type;
   function Generic_Kind_Test (Args : in Mal.T_Array) return Mal.T;
   function Generic_Kind_Test (Args : in Mal.T_Array) return Mal.T is
   begin
      Err.Check (Args'Length = 1, "expected 1 parameter");
      return (Kind_Boolean, Args (Args'First).Kind = Kind);
   end Generic_Kind_Test;

   generic
      with function Ada_Operator (Left, Right : in Integer) return Integer;
   function Generic_Mal_Operator (Args : in Mal.T_Array) return Mal.T;
   function Generic_Mal_Operator (Args : in Mal.T_Array) return Mal.T is
   begin
      Err.Check (Args'Length = 2, "expected 2 parameters");
      Err.Check (Args (Args'First).Kind = Kind_Number,
                 "parameter 1 must be a number");
      Err.Check (Args (Args'Last).Kind = Kind_Number,
                 "parameter 2 must be a number");
      return (Kind_Number, Ada_Operator (Args (Args'First).Number,
                                         Args (Args'Last).Number));
   end Generic_Mal_Operator;

   generic
      with function Ada_Operator (Left, Right : in Integer) return Boolean;
   function Generic_Comparison (Args : in Mal.T_Array) return Mal.T;
   function Generic_Comparison (Args : in Mal.T_Array) return Mal.T is
   begin
      Err.Check (Args'Length = 2, "expected 2 parameters");
      Err.Check (Args (Args'First).Kind = Kind_Number,
                 "parameter 1 must be a number");
      Err.Check (Args (Args'Last).Kind = Kind_Number,
                 "parameter 2 must be a number");
      return (Kind_Boolean, Ada_Operator (Args (Args'First).Number,
                                          Args (Args'Last).Number));
   end Generic_Comparison;

   function Addition      is new Generic_Mal_Operator ("+");
   function Apply         (Args : in Mal.T_Array) return Mal.T;
   function Division      is new Generic_Mal_Operator ("/");
   function Equals        (Args : in Mal.T_Array) return Mal.T;
   function Eval          (Args : in Mal.T_Array) return Mal.T;
   function Greater_Equal is new Generic_Comparison (">=");
   function Greater_Than  is new Generic_Comparison (">");
   function Is_Atom       is new Generic_Kind_Test (Kind_Atom);
   function Is_False      (Args : in Mal.T_Array) return Mal.T;
   function Is_Function   (Args : in Mal.T_Array) return Mal.T;
   function Is_Keyword    is new Generic_Kind_Test (Kind_Keyword);
   function Is_List       is new Generic_Kind_Test (Kind_List);
   function Is_Macro      is new Generic_Kind_Test (Kind_Macro);
   function Is_Map        is new Generic_Kind_Test (Kind_Map);
   function Is_Nil        is new Generic_Kind_Test (Kind_Nil);
   function Is_Number     is new Generic_Kind_Test (Kind_Number);
   function Is_Sequential (Args : in Mal.T_Array) return Mal.T;
   function Is_String     is new Generic_Kind_Test (Kind_String);
   function Is_Symbol     is new Generic_Kind_Test (Kind_Symbol);
   function Is_True       (Args : in Mal.T_Array) return Mal.T;
   function Is_Vector     is new Generic_Kind_Test (Kind_Vector);
   function Keyword       (Args : in Mal.T_Array) return Mal.T;
   function Less_Equal    is new Generic_Comparison ("<=");
   function Less_Than     is new Generic_Comparison ("<");
   function Mal_Do        (Args : in Mal.T_Array) return Mal.T;
   function Meta          (Args : in Mal.T_Array) return Mal.T;
   function Pr_Str        (Args : in Mal.T_Array) return Mal.T;
   function Println       (Args : in Mal.T_Array) return Mal.T;
   function Prn           (Args : in Mal.T_Array) return Mal.T;
   function Product       is new Generic_Mal_Operator ("*");
   function Read_String   (Args : in Mal.T_Array) return Mal.T;
   function Readline      (Args : in Mal.T_Array) return Mal.T;
   function Seq           (Args : in Mal.T_Array) return Mal.T;
   function Slurp         (Args : in Mal.T_Array) return Mal.T;
   function Str           (Args : in Mal.T_Array) return Mal.T;
   function Subtraction   is new Generic_Mal_Operator ("-");
   function Symbol        (Args : in Mal.T_Array) return Mal.T;
   function Time_Ms       (Args : in Mal.T_Array) return Mal.T;
   function With_Meta     (Args : in Mal.T_Array) return Mal.T;

   ----------------------------------------------------------------------

   function Apply (Args : in Mal.T_Array) return Mal.T is
   begin
      Err.Check (2 <= Args'Length, "expected at least 2 parameters");
      Err.Check (Args (Args'Last).Kind in Kind_Sequence,
                 "last parameter must be a sequence");
      declare
         use type Sequences.Ptr;
         F : Mal.T renames Args (Args'First);
         A : constant Mal.T_Array
           := Args (Args'First + 1 .. Args'Last - 1)
           & Args (Args'Last).Sequence;
      begin
         case F.Kind is
            when Kind_Builtin =>
               return F.Builtin.all (A);
            when Kind_Builtin_With_Meta =>
               return F.Builtin_With_Meta.Builtin.all (A);
            when Kind_Fn =>
               return F.Fn.Apply (A);
            when others =>
               Err.Raise_With ("parameter 1 must be a function");
         end case;
      end;
   end Apply;

   function Equals (Args : in Mal.T_Array) return Mal.T is
      use type Mal.T;
   begin
      Err.Check (Args'Length = 2, "expected 2 parameters");
      return (Kind_Boolean, Args (Args'First) = Args (Args'Last));
   end Equals;

   function Eval (Args : in Mal.T_Array) return Mal.T is
   begin
      Err.Check (Args'Length = 1, "expected 1 parameter");
      return Eval_Cb.Cb.all (Ast => Args (Args'First),
                             Env => Envs.Repl);
   end Eval;

   function Is_False (Args : in Mal.T_Array) return Mal.T is
   begin
      Err.Check (Args'Length = 1, "expected 1 parameter");
      return (Kind_Boolean, Args (Args'First).Kind = Kind_Boolean
                and then not Args (Args'First).Ada_Boolean);
   end Is_False;

   function Is_Function (Args : in Mal.T_Array) return Mal.T is
   begin
      Err.Check (Args'Length = 1, "expected 1 parameter");
      return (Kind_Boolean, Args (Args'First).Kind in Kind_Function);
   end Is_Function;

   function Is_Sequential (Args : in Mal.T_Array) return Mal.T is
   begin
      Err.Check (Args'Length = 1, "expected 1 parameter");
      return (Kind_Boolean, Args (Args'First).Kind in Kind_Sequence);
   end Is_Sequential;

   function Is_True (Args : in Mal.T_Array) return Mal.T is
   begin
      Err.Check (Args'Length = 1, "expected 1 parameter");
      return (Kind_Boolean, Args (Args'First).Kind = Kind_Boolean
                and then Args (Args'First).Ada_Boolean);
   end Is_True;

   function Keyword (Args : in Mal.T_Array) return Mal.T is
   begin
      Err.Check (Args'Length = 1, "expected 1 parameter");
      Err.Check (Args (Args'First).Kind = Kind_String, "expected a string");
      return (Kind_Keyword, Args (Args'First).S);
   end Keyword;

   function Mal_Do (Args : in Mal.T_Array) return Mal.T is
   begin
      Err.Check (0 < Args'Length, "expected at least 1 parameter");
      return Args (Args'Last);
   end Mal_Do;

   function Meta (Args : in Mal.T_Array) return Mal.T is
   begin
      Err.Check (Args'Length = 1, "expected 1 parameter");
      declare
         A1 : Mal.T renames Args (Args'First);
      begin
         case A1.Kind is
            when Kind_Sequence =>
               return A1.Sequence.Meta;
            when Kind_Map =>
               return A1.Map.Meta;
            when Kind_Fn =>
               return A1.Fn.Meta;
            when Kind_Builtin_With_Meta =>
               return A1.Builtin_With_Meta.Meta;
            when Kind_Builtin =>
               return Mal.Nil;
            when others =>
               Err.Raise_With ("expected a function, map or sequence");
         end case;
      end;
   end Meta;

   procedure NS_Add_To_Repl is
      procedure P (S : in Symbols.Ptr;
                   B : in Mal.Builtin_Ptr) with Inline;
      procedure P (S : in Symbols.Ptr;
                   B : in Mal.Builtin_Ptr)
      is
      begin
         Envs.Repl.Set (S, (Kind_Builtin, B));
      end P;
   begin
      P (Symbols.Constructor ("+"),           Addition'Access);
      P (Symbols.Constructor ("apply"),       Apply'Access);
      P (Symbols.Constructor ("assoc"),       Maps.Assoc'Access);
      P (Symbols.Constructor ("atom"),        Atoms.Atom'Access);
      P (Symbols.Constructor ("concat"),      Sequences.Concat'Access);
      P (Symbols.Constructor ("conj"),        Sequences.Conj'Access);
      P (Symbols.Constructor ("cons"),        Sequences.Cons'Access);
      P (Symbols.Constructor ("contains?"),   Maps.Contains'Access);
      P (Symbols.Constructor ("count"),       Sequences.Count'Access);
      P (Symbols.Names.Deref,                 Atoms.Deref'Access);
      P (Symbols.Constructor ("dissoc"),      Maps.Dissoc'Access);
      P (Symbols.Constructor ("/"),           Division'Access);
      P (Symbols.Constructor ("do"),          Mal_Do'Access);
      P (Symbols.Constructor ("="),           Equals'Access);
      P (Symbols.Constructor ("eval"),        Eval'Access);
      P (Symbols.Constructor ("first"),       Sequences.First'Access);
      P (Symbols.Constructor ("get"),         Maps.Get'Access);
      P (Symbols.Constructor (">="),          Greater_Equal'Access);
      P (Symbols.Constructor (">"),           Greater_Than'Access);
      P (Symbols.Constructor ("hash-map"),    Maps.Hash_Map'Access);
      P (Symbols.Constructor ("atom?"),       Is_Atom'Access);
      P (Symbols.Constructor ("empty?"),      Sequences.Is_Empty'Access);
      P (Symbols.Constructor ("false?"),      Is_False'Access);
      P (Symbols.Constructor ("fn?"),         Is_Function'Access);
      P (Symbols.Constructor ("keyword?"),    Is_Keyword'Access);
      P (Symbols.Constructor ("list?"),       Is_List'Access);
      P (Symbols.Constructor ("macro?"),      Is_Macro'Access);
      P (Symbols.Constructor ("map?"),        Is_Map'Access);
      P (Symbols.Constructor ("nil?"),        Is_Nil'Access);
      P (Symbols.Constructor ("number?"),     Is_Number'Access);
      P (Symbols.Constructor ("sequential?"), Is_Sequential'Access);
      P (Symbols.Constructor ("string?"),     Is_String'Access);
      P (Symbols.Constructor ("symbol?"),     Is_Symbol'Access);
      P (Symbols.Constructor ("true?"),       Is_True'Access);
      P (Symbols.Constructor ("vector?"),     Is_Vector'Access);
      P (Symbols.Constructor ("keys"),        Maps.Keys'Access);
      P (Symbols.Constructor ("keyword"),     Keyword'Access);
      P (Symbols.Constructor ("<="),          Less_Equal'Access);
      P (Symbols.Constructor ("<"),           Less_Than'Access);
      P (Symbols.Constructor ("list"),        Sequences.List'Access);
      P (Symbols.Constructor ("map"),         Sequences.Map'Access);
      P (Symbols.Constructor ("meta"),        Meta'Access);
      P (Symbols.Constructor ("nth"),         Sequences.Nth'Access);
      P (Symbols.Constructor ("pr-str"),      Pr_Str'Access);
      P (Symbols.Constructor ("println"),     Println'Access);
      P (Symbols.Constructor ("prn"),         Prn'Access);
      P (Symbols.Constructor ("*"),           Product'Access);
      P (Symbols.Constructor ("read-string"), Read_String'Access);
      P (Symbols.Constructor ("readline"),    Readline'Access);
      P (Symbols.Constructor ("reset!"),      Atoms.Reset'Access);
      P (Symbols.Constructor ("rest"),        Sequences.Rest'Access);
      P (Symbols.Constructor ("seq"),         Seq'Access);
      P (Symbols.Constructor ("slurp"),       Slurp'Access);
      P (Symbols.Constructor ("str"),         Str'Access);
      P (Symbols.Constructor ("-"),           Subtraction'Access);
      P (Symbols.Constructor ("swap!"),       Atoms.Swap'Access);
      P (Symbols.Constructor ("symbol"),      Symbol'Access);
      P (Symbols.Constructor ("throw"),       Err.Throw'Access);
      P (Symbols.Constructor ("time-ms"),     Time_Ms'Access);
      P (Symbols.Constructor ("vals"),        Maps.Vals'Access);
      P (Symbols.Constructor ("vector"),      Sequences.Vector'Access);
      P (Symbols.Names.With_Meta,             With_Meta'Access);
   end NS_Add_To_Repl;

   function Pr_Str (Args : in Mal.T_Array) return Mal.T is
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
      return (Kind_String, R);
   end Pr_Str;

   function Println (Args : in Mal.T_Array) return Mal.T is
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
      return Mal.Nil;
   end Println;

   function Prn (Args : in Mal.T_Array) return Mal.T is
   begin
      Ada.Text_IO.Unbounded_IO.Put_Line (Pr_Str (Args).S);
      return Mal.Nil;
   end Prn;

   function Readline (Args : in Mal.T_Array) return Mal.T is
   begin
      Err.Check (Args'Length = 1, "expected 1 parameter");
      Err.Check (Args (Args'First).Kind = Kind_String, "expected a string");
      Ada.Text_IO.Unbounded_IO.Put (Args (Args'First).S);
      if Ada.Text_IO.End_Of_File then
         return Mal.Nil;
      else
         return (Kind_String, Ada.Text_IO.Unbounded_IO.Get_Line);
      end if;
   end Readline;

   function Read_String (Args : in Mal.T_Array) return Mal.T is
   begin
      Err.Check (Args'Length = 1, "expected 1 parameter");
      Err.Check (Args (Args'First).Kind = Kind_String, "expected a string");
      declare
         R : constant Mal.T_Array
           := Reader.Read_Str (ASU.To_String (Args (Args'First).S));
      begin
         Err.Check (R'Length = 1, "parameter must contain 1 expression");
         return R (R'First);
      end;
   end Read_String;

   function Seq (Args : in Mal.T_Array) return Mal.T is
   begin
      Err.Check (Args'Length = 1, "expected 1 parameter");
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
                  return Sequences.List (R);
               end;
            end if;
         when Kind_Sequence =>
            if Args (Args'First).Sequence.Length = 0 then
               return Mal.Nil;
            else
               return (Kind_List, Args (Args'First).Sequence);
            end if;
         when others =>
            Err.Raise_With ("expected nil, a sequence or a string");
      end case;
   end Seq;

   function Slurp (Args : in Mal.T_Array) return Mal.T is
      use Ada.Text_IO;
      File   : File_Type;
      Buffer : ASU.Unbounded_String;
   begin
      Err.Check (Args'Length = 1, "expected 1 parameter");
      Err.Check (Args (Args'First).Kind = Kind_String, "expected a string");
      Open (File, In_File, ASU.To_String (Args (Args'First).S));
      while not End_Of_File (File) loop
         ASU.Append (Buffer, Get_Line (File));
         ASU.Append (Buffer, Ada.Characters.Latin_1.LF);
      end loop;
      Close (File);
      return (Kind_String, Buffer);
   exception
      when others =>
         Close (File);
         raise;
   end Slurp;

   function Str (Args : in Mal.T_Array) return Mal.T is
      R : ASU.Unbounded_String;
   begin
      for A of Args loop
         Printer.Pr_Str (R, A, Readably => False);
      end loop;
      return (Kind_String, R);
   end Str;

   function Symbol (Args : in Mal.T_Array) return Mal.T is
   begin
      Err.Check (Args'Length = 1, "expected 1 parameter");
      Err.Check (Args (Args'First).Kind = Kind_String, "expected a string");
      return (Kind_Symbol,
              Symbols.Constructor (ASU.To_String (Args (Args'First).S)));
   end Symbol;

   function Time_Ms (Args : in Mal.T_Array) return Mal.T is
      use type Ada.Calendar.Time;
   begin
      Err.Check (Args'Length = 0, "expected no parameter");
      return (Kind_Number,
              Integer (1000.0 * (Ada.Calendar.Clock - Start_Time)));
   end Time_Ms;

   function With_Meta (Args : in Mal.T_Array) return Mal.T is
   begin
      Err.Check (Args'Length = 2, "expected 2 parameters");
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
               return (Kind_List, A1.Sequence.With_Meta (A2));
            when Kind_Vector =>
               return (Kind_Vector, A1.Sequence.With_Meta (A2));
            when Kind_Map =>
               return A1.Map.With_Meta (A2);
            when Kind_Fn =>
               return A1.Fn.With_Meta (A2);
            when others =>
               Err.Raise_With
                 ("parameter 1 must be a function, map or sequence");
         end case;
      end;
   end With_Meta;

end Core;
