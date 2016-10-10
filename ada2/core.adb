with Ada.Calendar; use type Ada.Calendar.Time;
with Ada.Characters.Latin_1;
with Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO;
with Atoms; use type Atoms.Ptr;
with Lists;
with Maps;
with Names;
with Printer;
with Reader;
with Strings; use type Strings.Ptr;

package body Core is

   use Types;

   Start_Time : constant Ada.Calendar.Time := Ada.Calendar.Clock;

   Eval : Eval_Callback_Type;

   function Concatenation_Of_Pr_Str
     (Args           : in Mal_Type_Array;
      Print_Readably : in Boolean := True;
      Separator      : in String  := " ")
     return Ada.Strings.Unbounded.Unbounded_String;

   function Apply         (Args : in Mal_Type_Array) return Mal_Type;
   function Assoc         (Args : in Mal_Type_Array) return Mal_Type;
   function Atom          (Args : in Mal_Type_Array) return Mal_Type;
   function Concat        (Args : in Mal_Type_Array) return Mal_Type;
   function Conj          (Args : in Mal_Type_Array) return Mal_Type;
   function Cons          (Args : in Mal_Type_Array) return Mal_Type;
   function Contains      (Args : in Mal_Type_Array) return Mal_Type;
   function Count         (Args : in Mal_Type_Array) return Mal_Type;
   function Deref         (Args : in Mal_Type_Array) return Mal_Type;
   function Dissoc        (Args : in Mal_Type_Array) return Mal_Type;
   function Equals        (Args : in Mal_Type_Array) return Mal_Type;
   function First         (Args : in Mal_Type_Array) return Mal_Type;
   function Get           (Args : in Mal_Type_Array) return Mal_Type;
   function Hash_Map      (Args : in Mal_Type_Array) return Mal_Type;
   function Is_Empty      (Args : in Mal_Type_Array) return Mal_Type;
   function Is_False      (Args : in Mal_Type_Array) return Mal_Type;
   function Is_Sequential (Args : in Mal_Type_Array) return Mal_Type;
   function Is_True       (Args : in Mal_Type_Array) return Mal_Type;
   function Keys          (Args : in Mal_Type_Array) return Mal_Type;
   function Keyword       (Args : in Mal_Type_Array) return Mal_Type;
   function List          (Args : in Mal_Type_Array) return Mal_Type;
   function Map           (Args : in Mal_Type_Array) return Mal_Type;
   function Meta          (Args : in Mal_Type_Array) return Mal_Type;
   function Nth           (Args : in Mal_Type_Array) return Mal_Type;
   function Pr_Str        (Args : in Mal_Type_Array) return Mal_Type;
   function Println       (Args : in Mal_Type_Array) return Mal_Type;
   function Prn           (Args : in Mal_Type_Array) return Mal_Type;
   function Read_String   (Args : in Mal_Type_Array) return Mal_Type;
   function Readline      (Args : in Mal_Type_Array) return Mal_Type;
   function Reset         (Args : in Mal_Type_Array) return Mal_Type;
   function Rest          (Args : in Mal_Type_Array) return Mal_Type;
   function Seq           (Args : in Mal_Type_Array) return Mal_Type;
   function Slurp         (Args : in Mal_Type_Array) return Mal_Type;
   function Str           (Args : in Mal_Type_Array) return Mal_Type;
   function Swap          (Args : in Mal_Type_Array) return Mal_Type;
   function Symbol        (Args : in Mal_Type_Array) return Mal_Type;
   function Throw         (Args : in Mal_Type_Array) return Mal_Type;
   function Time_Ms       (Args : in Mal_Type_Array) return Mal_Type;
   function Vals          (Args : in Mal_Type_Array) return Mal_Type;
   function Vector        (Args : in Mal_Type_Array) return Mal_Type;
   function With_Meta     (Args : in Mal_Type_Array) return Mal_Type;

   generic
      with function Ada_Operator (Left, Right : in Integer) return Integer;
   function Generic_Mal_Operator (Args : in Mal_Type_Array) return Mal_Type;
   function Generic_Mal_Operator (Args : in Mal_Type_Array) return Mal_Type
   is (Kind_Number, Atoms.No_Element,
       Ada_Operator (Args (Args'First).Integer_Value,
                     Args (Args'First + 1).Integer_Value));
   function Addition    is new Generic_Mal_Operator ("+");
   function Subtraction is new Generic_Mal_Operator ("-");
   function Product     is new Generic_Mal_Operator ("*");
   function Division    is new Generic_Mal_Operator ("/");

   generic
      with function Ada_Operator (Left, Right : in Integer) return Boolean;
   function Generic_Comparison (Args : in Mal_Type_Array) return Mal_Type;
   function Generic_Comparison (Args : in Mal_Type_Array) return Mal_Type
   is (Kind_Boolean, Atoms.No_Element,
       Ada_Operator (Args (Args'First).Integer_Value,
                     Args (Args'First + 1).Integer_Value));
   function Greater_Than  is new Generic_Comparison (">");
   function Greater_Equal is new Generic_Comparison (">=");
   function Less_Than     is new Generic_Comparison ("<");
   function Less_Equal    is new Generic_Comparison ("<=");

   generic
      Kind : Kind_Type;
   function Generic_Kind_Test (Args : in Mal_Type_Array) return Mal_Type;
   function Generic_Kind_Test (Args : in Mal_Type_Array) return Mal_Type
   is (Kind_Boolean, Atoms.No_Element, Args (Args'First).Kind = Kind);
   function Is_Atom    is new Generic_Kind_Test (Kind_Atom);
   function Is_Keyword is new Generic_Kind_Test (Kind_Keyword);
   function Is_List    is new Generic_Kind_Test (Kind_List);
   function Is_Map     is new Generic_Kind_Test (Kind_Map);
   function Is_Nil     is new Generic_Kind_Test (Kind_Nil);
   function Is_String  is new Generic_Kind_Test (Kind_String);
   function Is_Symbol  is new Generic_Kind_Test (Kind_Symbol);
   function Is_Vector  is new Generic_Kind_Test (Kind_Vector);

   ----------------------------------------------------------------------

   procedure Add_Built_In_Functions
     (Repl          : in Environments.Ptr;
      Eval_Callback : in not null Eval_Callback_Type)
   is
      function N (N : in Native_Function_Access) return Mal_Type
        is (Kind_Native, Atoms.No_Element, N) with Inline;
   begin
      Eval := Eval_Callback;

      Repl.Increase_Capacity (57);

      Repl.Set (Names.Apply,         N (Apply'Access));
      Repl.Set (Names.Assoc,         N (Assoc'Access));
      Repl.Set (Names.Asterisk,      N (Product'Access));
      Repl.Set (Names.Atom,          N (Atom'Access));
      Repl.Set (Names.Concat,        N (Concat'Access));
      Repl.Set (Names.Conj,          N (Conj'Access));
      Repl.Set (Names.Cons,          N (Cons'Access));
      Repl.Set (Names.Contains,      N (Contains'Access));
      Repl.Set (Names.Count,         N (Count'Access));
      Repl.Set (Names.Deref,         N (Deref'Access));
      Repl.Set (Names.Dissoc,        N (Dissoc'Access));
      Repl.Set (Names.Equals,        N (Equals'Access));
      Repl.Set (Names.First,         N (First'Access));
      Repl.Set (Names.Get,           N (Get'Access));
      Repl.Set (Names.Greater_Equal, N (Greater_Equal'Access));
      Repl.Set (Names.Greater_Than,  N (Greater_Than'Access));
      Repl.Set (Names.Hash_Map,      N (Hash_Map'Access));
      Repl.Set (Names.Is_Atom,       N (Is_Atom'Access));
      Repl.Set (Names.Is_Empty,      N (Is_Empty'Access));
      Repl.Set (Names.Is_False,      N (Is_False'Access));
      Repl.Set (Names.Is_Keyword,    N (Is_Keyword'Access));
      Repl.Set (Names.Is_List,       N (Is_List'Access));
      Repl.Set (Names.Is_Map,        N (Is_Map'Access));
      Repl.Set (Names.Is_Nil,        N (Is_Nil'Access));
      Repl.Set (Names.Is_Sequential, N (Is_Sequential'Access));
      Repl.Set (Names.Is_String,     N (Is_String'Access));
      Repl.Set (Names.Is_Symbol,     N (Is_Symbol'Access));
      Repl.Set (Names.Is_True,       N (Is_True'Access));
      Repl.Set (Names.Is_Vector,     N (Is_Vector'Access));
      Repl.Set (Names.Keys,          N (Keys'Access));
      Repl.Set (Names.Keyword,       N (Keyword'Access));
      Repl.Set (Names.Less_Equal,    N (Less_Equal'Access));
      Repl.Set (Names.Less_Than,     N (Less_Than'Access));
      Repl.Set (Names.List,          N (List'Access));
      Repl.Set (Names.Map,           N (Map'Access));
      Repl.Set (Names.Meta,          N (Meta'Access));
      Repl.Set (Names.Minus,         N (Subtraction'Access));
      Repl.Set (Names.Nth,           N (Nth'Access));
      Repl.Set (Names.Plus,          N (Addition'Access));
      Repl.Set (Names.Pr_Str,        N (Pr_Str'Access));
      Repl.Set (Names.Println,       N (Println'Access));
      Repl.Set (Names.Prn,           N (Prn'Access));
      Repl.Set (Names.Read_String,   N (Read_String'Access));
      Repl.Set (Names.Readline,      N (Readline'Access));
      Repl.Set (Names.Reset,         N (Reset'Access));
      Repl.Set (Names.Rest,          N (Rest'Access));
      Repl.Set (Names.Seq,           N (Seq'Access));
      Repl.Set (Names.Slash,         N (Division'Access));
      Repl.Set (Names.Slurp,         N (Slurp'Access));
      Repl.Set (Names.Str,           N (Str'Access));
      Repl.Set (Names.Swap,          N (Swap'Access));
      Repl.Set (Names.Symbol,        N (Symbol'Access));
      Repl.Set (Names.Throw,         N (Throw'Access));
      Repl.Set (Names.Time_Ms,       N (Time_Ms'Access));
      Repl.Set (Names.Vals,          N (Vals'Access));
      Repl.Set (Names.Vector,        N (Vector'Access));
      Repl.Set (Names.With_Meta,     N (With_Meta'Access));
   end Add_Built_In_Functions;

   function Apply (Args : in Mal_Type_Array) return Mal_Type
   is
      Func    : Mal_Type  renames Args (Args'First);
      List    : Lists.Ptr renames Args (Args'Last).L;
      Actuals : Mal_Type_Array (1 .. Args'Length - 2 + List.Length);
   begin
      Actuals (1 .. Args'Length - 2) := Args (Args'First + 1 .. Args'Last - 1);
      for I in 1 .. List.Length loop
         Actuals (Args'Length - 2 + I) := List.Element (I);
      end loop;
      if Func.Kind = Kind_Native then
         return Func.Native.all (Actuals);
      else
         declare
            Env : constant Environments.Ptr
              := Environments.Alloc (Outer => Func.Environment);
         begin
            Env.Set_Binds (Func.Formals, Actuals);
            return Eval.all (Func.Expression.Deref, Env);
         end;
      end if;
   end Apply;

   function Assoc (Args : in Mal_Type_Array) return Mal_Type
   is (Kind_Map, Atoms.No_Element,
       Args (Args'First).Map.Assoc (Args (Args'First + 1 .. Args'Last)));

   function Atom (Args : in Mal_Type_Array) return Mal_Type
   is (Kind_Atom, Atoms.No_Element, Atoms.Alloc (Args (Args'First)));

   function Concat (Args : in Mal_Type_Array) return Mal_Type
   is
      L      : array (Args'Range) of Lists.Ptr;
      Sum    : Natural := 0;
      Result : Lists.Ptr;
   begin
      for I in Args'Range loop
         L (I) := Args (I).L;
         Sum := Sum + L (I).Length;
      end loop;
      Result := Lists.Alloc (Sum);
      Sum := 0;
      for LI of L loop
         for J in 1 .. LI.Length loop
            Sum := Sum + 1;
            Result.Replace_Element (Sum, LI.Element (J));
         end loop;
      end loop;
      return (Kind_List, Atoms.No_Element, Result);
   end Concat;

   function Concatenation_Of_Pr_Str
     (Args           : in Mal_Type_Array;
      Print_Readably : in Boolean := True;
      Separator      : in String  := " ")
     return Ada.Strings.Unbounded.Unbounded_String
   is
      use Ada.Strings.Unbounded;
      Result : Unbounded_String;
   begin
      if 1 <= Args'Length then
         Append (Result, Printer.Pr_Str (Args (Args'First), Print_Readably));
         for I in Args'First + 1 .. Args'Last loop
            Append (Result, Separator);
            Append (Result, Printer.Pr_Str (Args (I), Print_Readably));
         end loop;
      end if;
      return Result;
   end Concatenation_Of_Pr_Str;

   function Conj (Args : in Mal_Type_Array) return Mal_Type
   is
      List   : Lists.Ptr renames Args (Args'First).L;
      Result : constant Lists.Ptr
        := Lists.Alloc (List.Length + Args'Length - 1);
   begin
      if Args (Args'First).Kind = Kind_List then
         for I in Args'First + 1 .. Args'Last loop
            Result.Replace_Element (Args'Last + 1 - I, Args (I));
         end loop;
         for I in 1 .. List.Length loop
            Result.Replace_Element (Args'Length + I - 1, List.Element (I));
         end loop;
         return (Kind_List, Atoms.No_Element, Result);
      else
         for I in 1 .. Args'Length - 1 loop
            Result.Replace_Element (List.Length + I, Args (Args'First + I));
         end loop;
         for I in 1 .. List.Length loop
            Result.Replace_Element (I, List.Element (I));
         end loop;
         return (Kind_Vector, Atoms.No_Element, Result);
      end if;
   end Conj;

   function Cons (Args : in Mal_Type_Array) return Mal_Type
   is
      List   : Lists.Ptr renames Args (Args'First + 1).L;
      Result : constant Lists.Ptr := Lists.Alloc (1 + List.Length);
   begin
      Result.Replace_Element (1, Args (Args'First));
      for I in 1 .. List.Length loop
         Result.Replace_Element (I + 1, List.Element (I));
      end loop;
      return (Kind_List, Atoms.No_Element, Result);
   end Cons;

   function Contains (Args : in Mal_Type_Array) return Mal_Type
   is (Kind_Boolean, Atoms.No_Element,
       Args (Args'First).Map.Contains (Args (Args'First + 1)));

   function Count (Args : in Mal_Type_Array) return Mal_Type
   is (Kind_Number, Atoms.No_Element,
       (if Args (Args'First).Kind = Kind_Nil
          then 0
          else Args (Args'First).L.Length));

   function Deref (Args : in Mal_Type_Array) return Mal_Type
   is (Args (Args'First).Reference.Deref);

   function Dissoc (Args : in Mal_Type_Array) return Mal_Type
   is (Kind_Map, Atoms.No_Element,
       Args (Args'First).Map.Dissoc (Args (Args'First + 1 .. Args'Last)));

   function Equals (Args : in Mal_Type_Array) return Mal_Type
   is (Kind_Boolean, Atoms.No_Element,
       Args (Args'First) = Args (Args'First + 1));

   function First (Args : in Mal_Type_Array) return Mal_Type
   is (if Args (Args'First).Kind = Kind_Nil
         or else Args (Args'First).L.Length = 0
         then (Kind_Nil, Atoms.No_Element)
         else Args (Args'First).L.Element (1));

   function Get (Args : in Mal_Type_Array) return Mal_Type is
   begin
      if Args (Args'First).Kind = Kind_Nil then
         return (Kind_Nil, Atoms.No_Element);
      else
         return Args (Args'First).Map.Get (Args (Args'First + 1));
      end if;
   exception
      when Maps.Unknown_Key =>
         return (Kind_Nil, Atoms.No_Element);
   end Get;

   function Hash_Map (Args : in Mal_Type_Array) return Mal_Type
   is (Kind_Map, Atoms.No_Element, Maps.Hash_Map (Args));

   function Is_Empty (Args : in Mal_Type_Array) return Mal_Type
   is (Kind_Boolean, Atoms.No_Element, Args (Args'First).L.Length = 0);

   function Is_False (Args : in Mal_Type_Array) return Mal_Type
   is (Kind_Boolean, Atoms.No_Element,
       Args (Args'First).Kind = Kind_Boolean
         and then not Args (Args'First).Boolean_Value);

   function Is_True (Args : in Mal_Type_Array) return Mal_Type
   is (Kind_Boolean, Atoms.No_Element,
       Args (Args'First).Kind = Kind_Boolean
         and then Args (Args'First).Boolean_Value);

   function Is_Sequential (Args : in Mal_Type_Array) return Mal_Type
   is (Kind_Boolean, Atoms.No_Element,
       Args (Args'First).Kind in Kind_List | Kind_Vector);

   function Keyword (Args : in Mal_Type_Array) return Mal_Type
   is (Kind_Keyword, Atoms.No_Element, Args (Args'First).S);

   function Keys (Args : in Mal_Type_Array) return Mal_Type
   is
      M      : Maps.Ptr renames Args (Args'First).Map;
      Result : constant Mal_Type := (Kind_List, Atoms.No_Element,
                                     Lists.Alloc (M.Length));
      I      : Natural := 0;
      procedure Process (Key, Element : in Mal_Type);
      procedure Process (Key, Element : in Mal_Type) is
      begin
         I := I + 1;
         Result.L.Replace_Element (I, Key);
         pragma Unreferenced (Element);
      end Process;
   begin
      M.Iterate (Process'Access);
      return Result;
   end Keys;

   function List (Args : in Mal_Type_Array) return Mal_Type
   is (Kind_List, Atoms.No_Element, Lists.Alloc (Args));

   function Map (Args : in Mal_Type_Array) return Mal_Type
   is
      Func    : Mal_Type  renames Args (Args'First);
      List    : Lists.Ptr renames Args (Args'First + 1).L;
      Actuals : Mal_Type_Array (1 .. 1);
      Result  : constant Lists.Ptr := Lists.Alloc (List.Length);
   begin
      for I in 1 .. List.Length loop
         Actuals (1) := List.Element (I);
         if Func.Kind = Kind_Native then
            Result.Replace_Element (I, Func.Native.all (Actuals));
         else
            declare
               Env : constant Environments.Ptr
                 := Environments.Alloc (Func.Environment);
            begin
               Env.Set_Binds (Func.Formals, Actuals);
               Result.Replace_Element (I, Eval.all (Func.Expression.Deref,
                                                    Env));
            end;
         end if;
      end loop;
      return (Kind_List, Atoms.No_Element, Result);
   end Map;

   function Meta (Args : in Mal_Type_Array) return Mal_Type
   is (if Args (Args'First).Meta = Atoms.No_Element
         then (Kind_Nil, Atoms.No_Element)
         else Args (Args'First).Meta.Deref);

   function Nth (Args : in Mal_Type_Array) return Mal_Type
   is (Args (Args'First).L.Element (1 + Args (Args'First + 1).Integer_Value));

   function Pr_Str (Args : in Mal_Type_Array) return Mal_Type
   is (Kind_String, Atoms.No_Element, Strings.Alloc
         (Ada.Strings.Unbounded.To_String (Concatenation_Of_Pr_Str (Args))));

   function Println (Args : in Mal_Type_Array) return Mal_Type is
   begin
      Ada.Text_IO.Unbounded_IO.Put_Line (Concatenation_Of_Pr_Str
                                           (Args, Print_Readably => False));
      return (Kind_Nil, Atoms.No_Element);
   end Println;

   function Prn (Args : in Mal_Type_Array) return Mal_Type is
   begin
      Ada.Text_IO.Unbounded_IO.Put_Line (Concatenation_Of_Pr_Str (Args));
      return (Kind_Nil, Atoms.No_Element);
   end Prn;

   function Readline (Args : in Mal_Type_Array) return Mal_Type is
   begin
      Ada.Text_IO.Put (Args (Args'First).S.Deref);
      return (Kind_String, Atoms.No_Element,
              Strings.Alloc (Ada.Text_IO.Get_Line));
   exception
      when Ada.Text_IO.End_Error =>
         return (Kind_Nil, Atoms.No_Element);
   end Readline;

   function Read_String (Args : in Mal_Type_Array) return Mal_Type
   is (Reader.Read_Str (Args (Args'First).S.Deref));

   function Reset (Args : in Mal_Type_Array) return Mal_Type is
   begin
      Args (Args'First).Reference.Set (Args (Args'Last));
      return Args (Args'Last);
   end Reset;

   function Rest (Args : in Mal_Type_Array) return Mal_Type
   is
      List : Mal_Type renames Args (Args'First);
      Len  : Natural;
   begin
      return Result : Mal_Type (Kind_List) do
         if List.Kind /= Kind_Nil then
            Len := List.L.Length;
            if 0 < Len then
               Len := Len - 1;
               Result.L := Lists.Alloc (Len);
               for I in 1 .. Len loop
                  Result.L.Replace_Element (I, List.L.Element (I + 1));
               end loop;
            end if;
         end if;
      end return;
   end Rest;

   function Seq (Args : in Mal_Type_Array) return Mal_Type is
   begin
      if Args (Args'First).Kind = Kind_String then
         declare
            S      : constant String := Args (Args'First).S.Deref;
            Result : Lists.Ptr;
         begin
            if S'Length = 0 then
               return (Kind_Nil, Atoms.No_Element);
            else
               Result := Lists.Alloc (S'Length);
               for I in S'Range loop
                  Result.Replace_Element (I - S'First + 1, Mal_Type'
                                            (Kind_String, Atoms.No_Element,
                                             Strings.Alloc (S (I .. I))));
               end loop;
               return (Kind_List, Atoms.No_Element, Result);
            end if;
         end;
      elsif Args (Args'First).Kind = Kind_Nil
        or else Args (Args'First).L.Length = 0
      then
         return (Kind_Nil, Atoms.No_Element);
      else
         return (Kind_List, Atoms.No_Element, Args (Args'First).L);
      end if;
   end Seq;

   function Slurp (Args : in Mal_Type_Array) return Mal_Type
   is
      use Ada.Strings.Unbounded;
      use Ada.Text_IO;
      File   : File_Type;
      Buffer : Unbounded_String;
   begin
      Open (File, In_File, Args (Args'First).S.Deref);
      while not End_Of_File (File) loop
         Append (Buffer, Get_Line (File));
         Append (Buffer, Ada.Characters.Latin_1.LF);
      end loop;
      Close (File);
      return (Kind_String, Atoms.No_Element,
              Strings.Alloc (To_String (Buffer)));
   exception
      when others =>
         Close (File);
         raise;
   end Slurp;

   function Str (Args : in Mal_Type_Array) return Mal_Type
   is (Kind_String, Atoms.No_Element, Strings.Alloc
         (Ada.Strings.Unbounded.To_String
            (Concatenation_Of_Pr_Str (Args,
                                      Print_Readably => False,
                                      Separator      => ""))));

   function Swap (Args : in Mal_Type_Array) return Mal_Type
   is
      Atom    : Mal_Type renames Args (Args'First);
      Func    : Mal_Type renames Args (Args'First + 1);
      Actuals : Mal_Type_Array (Args'First + 1 .. Args'Last);
      Result  : Mal_Type;
   begin
      Actuals (Actuals'First) := Atom.Reference.Deref;
      for I in Actuals'First + 1 .. Args'Last loop
         Actuals (I) := Args (I);
      end loop;
      if Func.Kind = Kind_Native then
         Result := Func.Native.all (Actuals);
      else
         declare
            Env : constant Environments.Ptr
              := Environments.Alloc (Outer => Func.Environment);
         begin
            Env.Set_Binds (Func.Formals, Actuals);
            Result := Eval.all (Func.Expression.Deref, Env);
         end;
      end if;
      Atom.Reference.Set (Result);
      return Result;
   end Swap;

   function Symbol (Args : in Mal_Type_Array) return Mal_Type
   is (Kind_Symbol, Atoms.No_Element, Args (Args'First).S);

   function Throw (Args : in Mal_Type_Array) return Mal_Type is
   begin
      Last_Exception := Args (Args'First);
      raise Exception_Throwed;
      return (Kind_Nil, Atoms.No_Element); --  GNAT wants a return.
   end Throw;

   function Time_Ms (Args : in Mal_Type_Array) return Mal_Type
   is (Kind_Number, Atoms.No_Element,
       Integer (1000.0 * (Ada.Calendar.Clock - Start_Time)));

   function Vals (Args : in Mal_Type_Array) return Mal_Type
   is
      M      : Maps.Ptr renames Args (Args'First).Map;
      Result : constant Mal_Type := (Kind_List, Atoms.No_Element,
                                     Lists.Alloc (M.Length));
      I      : Natural := 0;
      procedure Process (Key, Element : in Mal_Type);
      procedure Process (Key, Element : in Mal_Type) is
      begin
         I := I + 1;
         Result.L.Replace_Element (I, Element);
         pragma Unreferenced (Key);
      end Process;
   begin
      M.Iterate (Process'Access);
      return Result;
   end Vals;

   function Vector (Args : in Mal_Type_Array) return Mal_Type
   is (Kind_Vector, Atoms.No_Element, Lists.Alloc (Args));

   function With_Meta (Args : in Mal_Type_Array) return Mal_Type is
   begin
      return Result : Mal_Type := Args (Args'First) do
         Result.Meta := Atoms.Alloc (Args (Args'First + 1));
      end return;
   end With_Meta;

end Core;
