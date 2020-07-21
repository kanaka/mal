with Err;
with Types.Fns;
with Types.Builtins;

package body Types.Sequences is

   function "=" (Left, Right : in Instance) return Boolean is
      --  Should become Left.all.Data = Right.all.Data when
      --  https://gcc.gnu.org/bugzilla/show_bug.cgi?id=89178 is fixed.
   begin
      return Left.Length = Right.Length
        and then
        (for all I in 1 .. Left.Data'Length => Left.Data (I) = Right.Data (I));
   end "=";

   function Concat (Args : in T_Array) return T is
      Sum   : Natural  := 0;
      First : Positive := 1;
      Last  : Natural;
   begin
      Err.Check ((for all A of Args => A.Kind in Kind_Sequence),
                 "expected sequences");
      for Arg of Args loop
         Sum := Sum + Arg.Sequence.all.Data'Length;
      end loop;
      declare
         Ref : constant Sequence_Ptr := Constructor (Sum);
      begin
         for Arg of Args loop
            Last := First - 1 + Arg.Sequence.all.Data'Last;
            Ref.all.Data (First .. Last) := Arg.Sequence.all.Data;
            First := Last + 1;
         end loop;
         return (Kind_List, Ref);
      end;
   end Concat;

   function Conj (Args : in T_Array) return T is
   begin
      Err.Check (0 < Args'Length, "expected at least 1 parameter");
      case Args (Args'First).Kind is
      when Kind_Sequence =>
         declare
            Data : T_Array renames Args (Args'First).Sequence.all.Data;
            Last : constant Natural := Args'Length - 1 + Data'Length;
            --  Avoid exceptions until Ref is controlled.
            Ref  : constant Sequence_Ptr := Constructor (Last);
         begin
            if Args (Args'First).Kind = Kind_List then
               for I in 1 .. Args'Length - 1 loop
                  Ref.all.Data (I) := Args (Args'Last - I + 1);
               end loop;
               Ref.all.Data (Args'Length .. Last) := Data;
               return (Kind_List, Ref);
            else
               Ref.all.Data := Data & Args (Args'First + 1 .. Args'Last);
               return (Kind_Vector, Ref);
            end if;
         end;
      when others =>
         Err.Raise_With ("parameter 1 must be a sequence");
      end case;
   end Conj;

   function Cons (Args : in T_Array) return T is
   begin
      Err.Check (Args'Length = 2
                   and then Args (Args'Last).Kind in Kind_Sequence,
                 "expected a value then a sequence");
      declare
         Head : T renames Args (Args'First);
         Tail : T_Array renames Args (Args'Last).Sequence.all.Data;
         Ref  : constant Sequence_Ptr := Constructor (1 + Tail'Length);
      begin
         Ref.all.Data := Head & Tail;
         return (Kind_List, Ref);
      end;
   end Cons;

   function Constructor (Length : in Natural) return Sequence_Ptr is
      Ref : constant Sequence_Ptr := new Instance (Length);
   begin
      Garbage_Collected.Register (Garbage_Collected.Pointer (Ref));
      return Ref;
   end Constructor;

   function Count (Args : in T_Array) return T is
   begin
      Err.Check (Args'Length = 1, "expected 1 parameter");
      case Args (Args'First).Kind is
      when Kind_Nil =>
         return (Kind_Number, 0);
      when Kind_Sequence =>
         return (Kind_Number, Args (Args'First).Sequence.all.Data'Length);
      when others =>
         Err.Raise_With ("parameter must be nil or a sequence");
      end case;
   end Count;

   function First (Args : in T_Array) return T is
   begin
      Err.Check (Args'Length = 1, "expected 1 parameter");
      case Args (Args'First).Kind is
      when Kind_Nil =>
         return Nil;
      when Kind_Sequence =>
         declare
            Data : T_Array renames Args (Args'First).Sequence.all.Data;
         begin
            if Data'Length = 0 then
               return Nil;
            else
               return Data (Data'First);
            end if;
         end;
      when others =>
         Err.Raise_With ("parameter must be nil or a sequence");
      end case;
   end First;

   function Is_Empty (Args : in T_Array) return T is
   begin
      Err.Check (Args'Length = 1
                   and then Args (Args'First).Kind in Kind_Sequence,
                 "expected a sequence");
      return (Kind_Boolean, Args (Args'First).Sequence.all.Data'Length = 0);
   end Is_Empty;

   procedure Keep_References (Object : in out Instance) is
   begin
      Keep (Object.Meta);
      for M of Object.Data loop
         Keep (M);
      end loop;
   end Keep_References;

   function List (Args : in T_Array) return T
   is
      Ref : constant Sequence_Ptr := Constructor (Args'Length);
   begin
      Ref.all.Data := Args;
      return (Kind_List, Ref);
   end List;

   function Map (Args : in T_Array) return T is
   begin
      Err.Check (Args'Length = 2
                   and then Args (Args'Last).Kind in Kind_Sequence,
                 "expected a function then a sequence");
      declare
         F   : T renames Args (Args'First);
         Src : T_Array renames Args (Args'Last).Sequence.all.Data;
         Ref : constant Sequence_Ptr := Constructor (Src'Length);
      begin
         case F.Kind is
            when Kind_Builtin =>
               for I in Src'Range loop
                  Ref.all.Data (I) := F.Builtin.all (Src (I .. I));
               end loop;
            when Kind_Builtin_With_Meta =>
               for I in Src'Range loop
                  Ref.all.Data (I)
                    := F.Builtin_With_Meta.all.Builtin.all (Src (I .. I));
               end loop;
            when Kind_Fn =>
               for I in Src'Range loop
                  Ref.all.Data (I) := F.Fn.all.Apply (Src (I .. I));
               end loop;
            when others =>
               Err.Raise_With ("parameter 1 must be a function");
         end case;
         return (Kind_List, Ref);
      end;
   end Map;

   function Nth (Args : in T_Array) return T is
   begin
      Err.Check (Args'Length = 2
                   and then Args (Args'First).Kind in Kind_Sequence
                   and then Args (Args'Last).Kind = Kind_Number,
                 "expected a sequence then a number");
      declare
         L : T_Array renames Args (Args'First).Sequence.all.Data;
         I : constant Integer := Args (Args'Last).Number + 1;
      begin
         Err.Check (I in L'Range, "index out of bounds");
         return L (I);
      end;
   end Nth;

   function Rest (Args : in T_Array) return T is
   begin
      Err.Check (Args'Length = 1, "expected 1 parameter");
      case Args (Args'First).Kind is
      when Kind_Nil =>
         return (Kind_List, Constructor (0));
      when Kind_Sequence =>
         declare
            A1  : T_Array renames Args (Args'First).Sequence.all.Data;
            Ref : constant Sequence_Ptr
              := Constructor (Integer'Max (0, A1'Length - 1));
         begin
            Ref.all.Data := A1 (A1'First + 1 .. A1'Last);
            return (Kind_List, Ref);
         end;
      when others =>
         Err.Raise_With ("parameter must be nil or a sequence");
      end case;
   end Rest;

   function Vec (Args : in T_Array) return T is
   begin
      Err.Check (Args'Length = 1
                   and then Args (Args'First).Kind in Kind_Sequence,
                 "expects a sequence");
      return (Kind_Vector, Args (Args'First).Sequence);
   end Vec;

   function Vector (Args : in T_Array) return T
   is
      Ref : constant Sequence_Ptr := Constructor (Args'Length);
   begin
      Ref.all.Data := Args;
      return (Kind_Vector, Ref);
   end Vector;

end Types.Sequences;
