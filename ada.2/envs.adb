with Ada.Containers.Hashed_Maps;
with Ada.Text_IO.Unbounded_IO;
with Ada.Unchecked_Deallocation;

with Err;
with Printer;
with Types.Sequences;
with Types.Symbols.Names;

package body Envs is

   use Types;

   --  The Eval built-in uses the REPL root environment (index 1),
   --  all others parameters only repeat the top index.

   package HM is new Ada.Containers.Hashed_Maps
     (Key_Type        => Symbols.Ptr,
      Element_Type    => Mal.T,
      Hash            => Symbols.Hash,
      Equivalent_Keys => Symbols."=",
      "="             => Mal."=");

   type Stack_Record
     (Outer_On_Stack    : Boolean     := True) is record
      Data              : HM.Map      := HM.Empty_Map;
      Refs              : Natural     := 1;
      --  Only references via the Ptr type.
      --  References from the stack or Alias are not counted here.
      Alias             : Heap_Access := null;
      --  Used by the closures and heap records to refer to this stack
      --  record, so that if it moves to the heap we only need to
      --  update the alias.
      case Outer_On_Stack is
         when True =>
            Outer_Index : Stack_Index := 0;
         when False =>
            Outer_Ref   : Heap_Access := null;
      end case;
   end record
     with Dynamic_Predicate => 0 < Refs
     and (Alias = null or else Alias.all.Outer = null)
     and (if Outer_On_Stack
            then Outer_Index <= Top
            else Outer_Ref /= null);

   --  It is forbidden to change the discriminant of an access type,
   --  so we cannot use a discriminant here.
   type Heap_Record is limited record
      Refs  : Natural     := 1;
      Data  : HM.Map      := HM.Empty_Map;
      Index : Stack_Index;
      Outer : Heap_Access := null;
   end record
     with Dynamic_Predicate =>
     (if Outer = null
        then Index in 1 .. Top and Data.Is_Empty
        else 0 < Refs);
        --  Either an alias for a stack element or an actual environment.

   --  There could be one single type, but this would enlarge the
   --  stack without simplifying the code, and prevent some more
   --  static type checking.

   Stack : array (Stack_Index range 1 .. Stack_Index'Last) of Stack_Record;
   --  The default value gives a consistent value to Stack (1),
   --  compatible with the Repl constant.

   procedure Free is new Ada.Unchecked_Deallocation (Heap_Record, Heap_Access);
   Allocations : Natural := 0;

   procedure Unreference (Reference : in out Heap_Access);

   procedure Set_Binds (M     : in out HM.Map;
                        Binds : in     Symbols.Symbol_Array;
                        Exprs : in     Mal.T_Array);

   ----------------------------------------------------------------------

   procedure Adjust (Object : in out Closure_Ptr) is
   begin
      if Object.Ref /= null then
         Object.Ref.all.Refs := @ + 1;
      end if;
   end Adjust;

   procedure Clear_And_Check_Allocations is
   begin
      pragma Assert (Top = 1);
      pragma Assert (Stack (1).Refs = 1);
      Stack (1).Data.Clear;
      if Stack (1).Alias /= null then
         if Stack (1).Alias.all.Refs /= 0 then
            Dump_Stack (Long => True);
         end if;
         pragma Assert (Stack (1).Alias.all.Refs = 0);
         Allocations := Allocations - 1;
         Free (Stack (1).Alias);
      end if;
      pragma Assert (Allocations = 0);
   end Clear_And_Check_Allocations;

   function Copy_Pointer (Env : in Ptr) return Ptr is
      pragma Assert (Env.Index in 1 | Top);
   begin
      Stack (Env.Index).Refs := @ + 1;
      return (Ada.Finalization.Limited_Controlled with Env.Index);
   end Copy_Pointer;

   procedure Dump_Stack (Long : in Boolean) is
      use Ada.Text_IO;
   begin
      for I in 1 .. Top loop
         if Long then
            Put ("Level");
         end if;
         Put (I'Img);
         if Long then
            New_Line;
            Put_Line ("  refs=" & Stack (I).Refs'Img);
            if Stack (I).Alias = null then
               Put_Line ("  no alias");
            else
               Put_Line ("  an alias with" & Stack (I).Alias.all.Refs'Img
                           & " refs");
            end if;
         end if;
         if Long then
            Put ("  outer=");
         else
            Put (" (->");
         end if;
         if Stack (I).Outer_On_Stack then
            Put (Stack (I).Outer_Index'Img);
         elsif Stack (I).Outer_Ref.all.Outer = null then
            if Long then
               Put ("alias for ");
            end if;
            Put (Stack (I).Outer_Ref.all.Index'Img);
         else
            Put (" closure for ex " & Stack (I).Outer_Ref.all.Index'Img);
         end if;
         if Long then
            New_Line;
         else
            Put ("):");
         end if;
         for P in Stack (I).Data.Iterate loop
            if HM.Element (P).Kind /= Kind_Builtin or 1 < I then
               Put ("   ");
               Put (HM.Key (P).To_String);
               Put (':');
               Unbounded_IO.Put (Printer.Pr_Str (HM.Element (P)));
               if Long then
                  New_Line;
               end if;
            end if;
         end loop;
         New_Line;
      end loop;
   end Dump_Stack;

   procedure Finalize (Object : in out Closure_Ptr) is
   begin
      Unreference (Object.Ref);
   end Finalize;

   procedure Finalize (Object : in out Ptr) is
   begin
      if 0 < Object.Index then
         if 0 < Stack (Object.Index).Refs then
            Stack (Object.Index).Refs := @ - 1;
         end if;
         Object.Index := 0;

         --  If Index = Top and there are no more references.
         loop
            pragma Assert (0 < Top);
            declare
               R : Stack_Record renames Stack (Top);
            begin
               exit when 0 < R.Refs;

               if Top = 1 then
                  R.Data.Clear;
                  if R.Alias /= null then
                     pragma Assert (R.Alias.all.Outer = null);
                     pragma Assert (R.Alias.all.Refs = 0);
                     Allocations := Allocations - 1;
                     Free (R.Alias);
                  end if;
                  exit;
               elsif R.Alias = null then
                  R.Data.Clear;
                  if not R.Outer_On_Stack then
                     Unreference (R.Outer_Ref);
                  end if;
               elsif R.Alias.all.Refs = 0 then
                  pragma Assert (R.Alias.all.Outer = null);
                  Allocations := Allocations - 1;
                  Free (R.Alias);
                  R.Data.Clear;
                  if not R.Outer_On_Stack then
                     Unreference (R.Outer_Ref);
                  end if;
               else
                  --  Detach this environment from the stack.

                  --  The reference count is already correct.

                  --  Copy the hashmap contents without reallocation..
                  R.Alias.all.Data.Move (R.Data);

                  --  The Index will not be used anymore.

                  --  We need the parent to have an alias, in case it
                  --  must be detached later.
                  if R.Outer_On_Stack then
                     declare
                        O : Stack_Record renames Stack (R.Outer_Index);
                     begin
                        if O.Alias = null then
                           Allocations := Allocations + 1;
                           O.Alias := new Heap_Record'(Index  => R.Outer_Index,
                                                       others => <>);
                        else
                           O.Alias.all.Refs := @ + 1;
                        end if;
                        R.Alias.all.Outer := O.Alias;
                     end;
                  else
                     R.Alias.all.Outer := R.Outer_Ref;
                  end if;
                  R.Alias := null;
               end if;
            end;
            Top := Top - 1;
         end loop;
      end if;
   end Finalize;

   function Get (Evt : in Ptr;
                 Key : in Symbols.Ptr) return Mal.T
   is
      pragma Assert (Evt.Index in 1 | Top);
      Index      : Stack_Index := Evt.Index;
      Ref        : Heap_Access;
      Definition : HM.Cursor;
   begin
      Main_Loop : loop
         Index_Loop : loop
            Definition := Stack (Index).Data.Find (Key);
            if HM.Has_Element (Definition) then
               return HM.Element (Definition);
            end if;
            exit Index_Loop when not Stack (Index).Outer_On_Stack;
            Index := Stack (Index).Outer_Index;
            exit Main_Loop when Index = 0;
         end loop Index_Loop;
         Ref := Stack (Index).Outer_Ref;
         Ref_Loop : loop
            Definition := Ref.all.Data.Find (Key);
            if HM.Has_Element (Definition) then
               return HM.Element (Definition);
            end if;
            exit Ref_Loop when Ref.all.Outer = null;
            Ref := Ref.all.Outer;
         end loop Ref_Loop;
         Index := Ref.all.Index;
      end loop Main_Loop;
      Err.Raise_With ("'" & Key.To_String & "' not found");
   end Get;

   function New_Closure (Env : in Ptr'Class) return Closure_Ptr is
      pragma Assert (Env.Index in 1 | Top);
      Alias : Heap_Access renames Stack (Env.Index).Alias;
   begin
      if Alias = null then
         Allocations := Allocations + 1;
         Alias := new Heap_Record'(Index => Env.Index, others => <>);
      else
         Alias.all.Refs := @ + 1;
      end if;
      return (Ada.Finalization.Controlled with Alias);
   end New_Closure;

   procedure Replace_With_Sub (Env : in out Ptr) is
      pragma Assert (Env.Index in 1 | Top);
      R : Stack_Record renames Stack (Env.Index);
   begin
      if Env.Index < Top or 1 < R.Refs
        or (R.Alias /= null and then 0 < R.Alias.all.Refs)
      then
         R.Refs := @ - 1;
         Top := Top + 1;
         pragma Assert (Stack (Top).Data.Is_Empty);
         pragma Assert (Stack (Top).Alias = null);
         Stack (Top) := (Outer_Index => Env.Index,
                         others      => <>);
         Env.Index := Top;
      end if;
      --  Else reuse the top stack record, including its map and its
      --  unreferenced alias if any.
   end Replace_With_Sub;

   procedure Replace_With_Sub (Env   : in out Ptr;
                               Outer : in     Closure_Ptr'Class;
                               Binds : in     Symbols.Symbol_Array;
                               Exprs : in     Mal.T_Array)
   is
      pragma Assert (Env.Index in 1 | Top);
   begin
      --  Finalize Env before creating the new environment, in case
      --  this is the last reference and it can be forgotten.
      --  Automatic assignment would construct the new value before
      --  finalizing the old one.
      Finalize (Env);
      Outer.Ref.all.Refs := @ + 1;
      Top := Top + 1;
      pragma Assert (Stack (Top).Data.Is_Empty);
      pragma Assert (Stack (Top).Alias = null);
      Stack (Top) := (Outer_On_Stack => False,
                      Outer_Ref      => Outer.Ref,
                      others         => <>);
      Env.Index := Top;
      --  Now we can afford raising exceptions.
      Set_Binds (Stack (Top).Data, Binds, Exprs);
   end Replace_With_Sub;

   procedure Replace_With_Sub (Env   : in out Ptr;
                               Binds : in     Symbols.Symbol_Array;
                               Exprs : in     Mal.T_Array)
   is
      pragma Assert (Env.Index in 1 | Top);
   begin
      Replace_With_Sub (Env);
      Set_Binds (Stack (Top).Data, Binds, Exprs);
   end Replace_With_Sub;

   procedure Set (Env         : in Ptr;
                  Key         : in Symbols.Ptr;
                  New_Element : in Mal.T)
   is
      pragma Assert (Env.Index in 1 | Top);
   begin
      Stack (Env.Index).Data.Include (Key, New_Element);
   end Set;

   procedure Set_Binds (M     : in out HM.Map;
                        Binds : in     Symbols.Symbol_Array;
                        Exprs : in     Mal.T_Array)
   is
      use type Symbols.Ptr;
      Varargs : constant Boolean := 1 < Binds'Length and then
        Binds (Binds'Last - 1) = Symbols.Names.Ampersand;
   begin
      Err.Check ((if Varargs then Binds'Length - 2 <= Exprs'Length
                             else Exprs'Length = Binds'Length),
                 "actual parameters do not match formal parameters");
      for I in 0 .. Binds'Length - (if Varargs then 3 else 1) loop
         M.Include (Binds (Binds'First + I), Exprs (Exprs'First + I));
      end loop;
      if Varargs then
         M.Include (Binds (Binds'Last), Sequences.List
                      (Exprs (Exprs'First + Binds'Length - 2 .. Exprs'Last)));
      end if;
   end Set_Binds;

   function Sub (Outer : in Ptr;
                 Binds : in Symbols.Symbol_Array;
                 Exprs : in Mal.T_Array) return Ptr
   is
      pragma Assert (Outer.Index in 1 | Top);
   begin
      Top := Top + 1;
      pragma Assert (Stack (Top).Data.Is_Empty);
      pragma Assert (Stack (Top).Alias = null);
      Stack (Top) := (Outer_Index => Outer.Index,
                      others      => <>);
      Set_Binds (Stack (Top).Data, Binds, Exprs);
      return (Ada.Finalization.Limited_Controlled with Top);
   end Sub;

   function Sub (Outer : in Closure_Ptr'Class;
                 Binds : in Symbols.Symbol_Array;
                 Exprs : in Mal.T_Array) return Ptr
   is
   begin
      Outer.Ref.all.Refs := @ + 1;
      Top := Top + 1;
      pragma Assert (Stack (Top).Data.Is_Empty);
      pragma Assert (Stack (Top).Alias = null);
      Stack (Top) := (Outer_On_Stack => False,
                      Outer_Ref      => Outer.Ref,
                      others         => <>);
      --  Take care to construct the result before raising any
      --  exception, so that it is finalized correctly.
      return R : constant Ptr := (Ada.Finalization.Limited_Controlled with Top)
      do
         --  Now we can afford raising exceptions.
         Set_Binds (Stack (Top).Data, Binds, Exprs);
      end return;
   end Sub;

   procedure Unreference (Reference : in out Heap_Access) is
      Ref : Heap_Access := Reference;
   begin
      Reference := null;
      loop
         exit when Ref = null;
         exit when Ref.all.Refs = 0;
         Ref.all.Refs := @ - 1;
         exit when 0 < Ref.all.Refs;
         exit when Ref.all.Outer = null; -- An alias.  Do not free it
         --  now, it may be useful for another closure.
         declare
            Tmp : Heap_Access := Ref;
         begin
            Ref := Ref.all.Outer;
            Allocations := Allocations - 1;
            Free (Tmp);
            pragma Unreferenced (Tmp);
         end;
      end loop;
   end Unreference;

end Envs;
