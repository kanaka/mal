with Err;
with Types.Builtins;
with Types.Fns;

package body Types.Sequences is

   use type Mal.T_Array;

   ----------------------------------------------------------------------

   function "=" (Left, Right : in Instance) return Boolean is
      --  Should become Left.Ref.all.Data = Right.Ref.all.Data when
      --  https://gcc.gnu.org/bugzilla/show_bug.cgi?id=89178 is fixed.
      use type Mal.T;
   begin
      return Left.Last = Right.Last
        and then
        (for all I in 1 .. Left.Last => Left.Data (I) = Right.Data (I));
   end "=";

   function "&" (Left  : in Mal.T_Array;
                 Right : in Instance) return Mal.T_Array
   is (Left & Right.Data);

   function Concat (Args : in Mal.T_Array) return Mal.T is
      Sum   : Natural  := 0;
      First : Positive := 1;
      Last  : Natural;
      Ref   : Mal.Sequence_Ptr;
   begin
      for Arg of Args loop
         Err.Check (Arg.Kind in Kind_Sequence, "expected sequences");
         Sum := Sum + Arg.Sequence.all.Data'Length;
      end loop;
      Ref := Constructor (Sum);
      for Arg of Args loop
         Last := First - 1 + Arg.Sequence.all.Data'Length;
         Ref.all.Data (First .. Last) := Arg.Sequence.all.Data;
         First := Last + 1;
      end loop;
      return (Kind_List, Ref);
   end Concat;

   function Conj (Args : in Mal.T_Array) return Mal.T is
   begin
      Err.Check (0 < Args'Length, "expected at least 1 parameter");
      case Args (Args'First).Kind is
      when Kind_Sequence =>
         declare
            Data : Mal.T_Array renames Args (Args'First).Sequence.all.Data;
            Last : constant Natural := Args'Length - 1 + Data'Length;
            --  Avoid exceptions until Ref is controlled.
            Ref  : constant Mal.Sequence_Ptr := Constructor (Last);
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

   function Cons (Args : in Mal.T_Array) return Mal.T is
   begin
      Err.Check (Args'Length = 2, "expected 2 parameters");
      Err.Check (Args (Args'Last).Kind in Kind_Sequence,
                 "parameter 2 must be a sequence");
      declare
         Head : Mal.T renames Args (Args'First);
         Tail : Mal.T_Array renames Args (Args'Last).Sequence.all.Data;
         Ref  : constant Mal.Sequence_Ptr := Constructor (1 + Tail'Length);
      begin
         Ref.all.Data := Head & Tail;
         return (Kind_List, Ref);
      end;
   end Cons;

   function Constructor (Length : in Natural) return Mal.Sequence_Ptr is
      Ref : constant Mal.Sequence_Ptr := new Instance (Length);
   begin
      Garbage_Collected.Register (Garbage_Collected.Pointer (Ref));
      return Ref;
   end Constructor;

   function Count (Args : in Mal.T_Array) return Mal.T is
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

   function Element (Container : in Instance;
                     Index     : in Positive) return Mal.T
   is (Container.Data (Index));

   function First (Args : in Mal.T_Array) return Mal.T is
   begin
      Err.Check (Args'Length = 1, "expected 1 parameter");
      case Args (Args'First).Kind is
      when Kind_Nil =>
         return Mal.Nil;
      when Kind_Sequence =>
         declare
            Data : Mal.T_Array renames Args (Args'First).Sequence.all.Data;
         begin
            if Data'Length = 0 then
               return Mal.Nil;
            else
               return Data (Data'First);
            end if;
         end;
      when others =>
         Err.Raise_With ("parameter must be nil or a sequence");
      end case;
   end First;

   function Is_Empty (Args : in Mal.T_Array) return Mal.T is
   begin
      Err.Check (Args'Length = 1, "expected 1 parameter");
      Err.Check (Args (Args'First).Kind in Kind_Sequence,
                 "parameter must be a sequence");
      return (Kind_Boolean, Args (Args'First).Sequence.all.Data'Length = 0);
   end Is_Empty;

   procedure Keep_References (Object : in out Instance) is
   begin
      Mal.Keep (Object.F_Meta);
      for M of Object.Data loop
         Mal.Keep (M);
      end loop;
   end Keep_References;

   function Length (Source : in Instance) return Natural
   is (Source.Data'Length);

   function List (Args : in Mal.T_Array) return Mal.T is
      Ref : constant Mal.Sequence_Ptr := Constructor (Args'Length);
   begin
      Ref.all.Data := Args;
      return (Kind_List, Ref);
   end List;

   function Map (Args : in Mal.T_Array) return Mal.T is
   begin
      Err.Check (Args'Length = 2, "expected 2 parameters");
      Err.Check (Args (Args'Last).Kind in Kind_Sequence,
                 "parameter 2 must be a sequence");
      declare
         F   : Mal.T renames Args (Args'First);
         Src : Mal.T_Array renames Args (Args'Last).Sequence.all.Data;
         Ref : constant Mal.Sequence_Ptr := Constructor (Src'Length);
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

   function Meta (Item : in Instance) return Mal.T
   is (Item.F_Meta);

   function Nth (Args : in Mal.T_Array) return Mal.T is
   begin
      Err.Check (Args'Length = 2, "expected 2 parameters");
      Err.Check (Args (Args'First).Kind in Kind_Sequence,
                 "paramater 1 must be a sequence");
      Err.Check (Args (Args'Last).Kind = Kind_Number,
                 "parameter 2 must be a number");
      declare
         L : Mal.T_Array renames Args (Args'First).Sequence.all.Data;
         I : constant Integer := Args (Args'Last).Number + 1;
      begin
         Err.Check (I in L'Range, "index out of bounds");
         return L (I);
      end;
   end Nth;

   procedure Replace_Element (Container : in out Instance;
                              Index     : in     Positive;
                              New_Item  : in     Mal.T)
   is
   begin
      Container.Data (Index) := New_Item;
   end Replace_Element;

   function Rest (Args : in Mal.T_Array) return Mal.T is
   begin
      Err.Check (Args'Length = 1, "expected 1 parameter");
      declare
         A1  : Mal.T renames Args (Args'First);
         Ref : Mal.Sequence_Ptr;
      begin
         --  Avoid exceptions until Ref is controlled.
         case A1.Kind is
            when Kind_Nil =>
               Ref := Constructor (0);
            when Kind_Sequence =>
               if A1.Sequence.all.Last = 0 then
                  Ref := Constructor (0);
               else
                  Ref := Constructor (A1.Sequence.all.Last - 1);
                  Ref.all.Data
                    := A1.Sequence.all.Data (2 .. A1.Sequence.all.Data'Last);
               end if;
            when others =>
               Err.Raise_With ("parameter must be nil or a sequence");
         end case;
         return (Kind_List, Ref);
      end;
   end Rest;

   function Tail (Source : in Instance;
                  Count  : in Natural) return Mal.T_Array is
      Data : Mal.T_Array renames Source.Data;
   begin
      return Data (Data'Last - Count + 1 .. Data'Last);
   end Tail;

   function Vector (Args : in Mal.T_Array) return Mal.T is
      Ref : constant Mal.Sequence_Ptr := Constructor (Args'Length);
   begin
      Ref.all.Data := Args;
      return (Kind_Vector, Ref);
   end Vector;

   function With_Meta (Data     : in Instance;
                       Metadata : in Mal.T) return Mal.Sequence_Ptr
   is
      Ref : constant Mal.Sequence_Ptr := Constructor (Data.Last);
   begin
      Ref.all.Data := Data.Data;
      Ref.all.F_Meta := Metadata;
      return Ref;
   end With_Meta;

end Types.Sequences;
