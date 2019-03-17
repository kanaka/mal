with Ada.Unchecked_Deallocation;

with Err;
with Types.Mal;

package body Types.Sequences is

   subtype AFC is Ada.Finalization.Controlled;
   use type Mal.T_Array;

   type Rec (Last : Natural) is limited record
      Refs : Natural                 := 1;
      Meta : Mal.T                   := Mal.Nil;
      Data : Mal.T_Array (1 .. Last) := (others => Mal.Nil);
   end record;

   procedure Free is new Ada.Unchecked_Deallocation (Rec, Acc);
   Allocations : Natural := 0;

   ----------------------------------------------------------------------

   function "=" (Left, Right : in Ptr) return Boolean is
      --  Should become Left.Ref.all.Data = Right.Ref.all.Data when
      --  https://gcc.gnu.org/bugzilla/show_bug.cgi?id=89178 is fixed.
      use type Mal.T;
      L : Rec renames Left.Ref.all;
      R : Rec renames Right.Ref.all;
   begin
      return L.Last = R.Last
        and then (for all I in 1 .. L.Last => L.Data (I) = R.Data (I));
   end "=";

   function "&" (Left  : in Mal.T_Array;
                 Right : in Ptr) return Mal.T_Array
   is (Left & Right.Ref.all.Data);

   procedure Adjust (Object : in out Ptr) is
   begin
      Object.Ref.all.Refs := @ + 1;
   end Adjust;

   procedure Check_Allocations is
   begin
      pragma Assert (Allocations = 0);
   end Check_Allocations;

   function Concat (Args : in Mal.T_Array) return Mal.T is
      Sum   : Natural  := 0;
      First : Positive := 1;
      Last  : Natural;
      Ref   : Acc;
   begin
      for Arg of Args loop
         Err.Check (Arg.Kind in Kind_Sequence, "expected sequences");
         Sum := Sum + Arg.Sequence.Ref.all.Data'Length;
      end loop;
      Allocations := Allocations + 1;
      --  Avoid exceptions until Ref is controlled.
      Ref := new Rec (Sum);
      for Arg of Args loop
         Last := First - 1 + Arg.Sequence.Ref.all.Data'Length;
         Ref.all.Data (First .. Last) := Arg.Sequence.Ref.all.Data;
         First := Last + 1;
      end loop;
      return (Kind_List, (AFC with Ref));
   end Concat;

   function Conj (Args : in Mal.T_Array) return Mal.T is
   begin
      Err.Check (0 < Args'Length, "expected at least 1 parameter");
      case Args (Args'First).Kind is
      when Kind_Sequence =>
         declare
            Data : Mal.T_Array renames Args (Args'First).Sequence.Ref.all.Data;
            Last : constant Natural := Args'Length - 1 + Data'Length;
            --  Avoid exceptions until Ref is controlled.
            Ref  : constant Acc := new Rec (Last);
         begin
            Allocations := Allocations + 1;
            if Args (Args'First).Kind = Kind_List then
               for I in 1 .. Args'Length - 1 loop
                  Ref.all.Data (I) := Args (Args'Last - I + 1);
               end loop;
               Ref.all.Data (Args'Length .. Last) := Data;
               return (Kind_List, (AFC with Ref));
            else
               Ref.all.Data := Data & Args (Args'First + 1 .. Args'Last);
               return (Kind_Vector, (AFC with Ref));
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
         Tail : Mal.T_Array renames Args (Args'Last).Sequence.Ref.all.Data;
      begin
         Allocations := Allocations + 1;
         return (Kind_List, (AFC with new Rec'(Last   => 1 + Tail'Length,
                                               Data   => Head & Tail,
                                               others => <>)));
      end;
   end Cons;

   function Count (Args : in Mal.T_Array) return Mal.T is
   begin
      Err.Check (Args'Length = 1, "expected 1 parameter");
      case Args (Args'First).Kind is
      when Kind_Nil =>
         return (Kind_Number, 0);
      when Kind_Sequence =>
         return (Kind_Number, Args (Args'First).Sequence.Ref.all.Data'Length);
      when others =>
         Err.Raise_With ("parameter must be nil or a sequence");
      end case;
   end Count;

   function Element (Container : in Ptr;
                     Index     : in Positive) return Mal.T
   is (Container.Ref.all.Data (Index));

   procedure Finalize (Object : in out Ptr) is
   begin
      if Object.Ref /= null and then 0 < Object.Ref.all.Refs then
         Object.Ref.all.Refs := @ - 1;
         if 0 < Object.Ref.all.Refs then
            Object.Ref := null;
         else
            Allocations := Allocations - 1;
            Free (Object.Ref);
         end if;
      end if;
   end Finalize;

   function First (Args : in Mal.T_Array) return Mal.T is
   begin
      Err.Check (Args'Length = 1, "expected 1 parameter");
      case Args (Args'First).Kind is
      when Kind_Nil =>
         return Mal.Nil;
      when Kind_Sequence =>
         declare
            Data : Mal.T_Array renames Args (Args'First).Sequence.Ref.all.Data;
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

   function Generic_Eval (Container : in Ptr;
                          Env       : in Env_Type)
                         return Ptr
   is
      --  Avoid exceptions until Ref is controlled.
      Ref : Acc := Container.Ref;
   begin
      pragma Assert (0 < Ref.all.Refs);
      if Ref.all.Refs = 1 then
         Ref.all.Refs := 2;
         Ref.all.Meta := Mal.Nil;
      else
         Allocations := Allocations + 1;
         Ref := new Rec (Ref.all.Last);
      end if;
      return R : constant Ptr := (AFC with Ref) do
         for I in Container.Ref.all.Data'Range loop
            Ref.all.Data (I) := Eval (Container.Ref.all.Data (I), Env);
            --  This call may raise exceptions.
            --  The target may be the source.
         end loop;
      end return;
   end Generic_Eval;

   function Is_Empty (Args : in Mal.T_Array) return Mal.T is
   begin
      Err.Check (Args'Length = 1, "expected 1 parameter");
      Err.Check (Args (Args'First).Kind in Kind_Sequence,
                 "parameter must be a sequence");
      return (Kind_Boolean,
              Args (Args'First).Sequence.Ref.all.Data'Length = 0);
   end Is_Empty;

   function Length (Source : in Ptr) return Natural
   is (Source.Ref.all.Data'Length);

   function List (Args : in Mal.T_Array) return Mal.T is
   begin
      Allocations := Allocations + 1;
      return (Kind_List, (AFC with new Rec'(Data   => Args,
                                            Last   => Args'Length,
                                            others => <>)));
   end List;

   function Map (Args : in Mal.T_Array) return Mal.T is
   begin
      Err.Check (Args'Length = 2, "expected 2 parameters");
      Err.Check (Args (Args'Last).Kind in Kind_Sequence,
                 "parameter 2 must be a sequence");
      declare
         F   : Mal.T renames Args (Args'First);
         Src : Mal.T_Array renames Args (Args'Last).Sequence.Ref.all.Data;
         --  Avoid exceptions until Ref is controlled.
         Ref : Acc := Args (Args'Last).Sequence.Ref;
      begin
         pragma Assert (0 < Ref.all.Refs);
         if Ref.all.Refs = 1 then
            Ref.all.Refs := 2;
            Ref.all.Meta := Mal.Nil;
         else
            Allocations := Allocations + 1;
            Ref := new Rec (Ref.all.Last);
         end if;
         return R : constant Mal.T := (Kind_List, (AFC with Ref)) do
            case F.Kind is
            when Kind_Builtin =>
               for I in Src'Range loop
                  Ref.all.Data (I) := F.Builtin.all (Src (I .. I));
                  --  This call may raise exceptions.
                  --  The target may be the same storage than the source.
               end loop;
            when Kind_Builtin_With_Meta =>
               for I in Src'Range loop
                  Ref.all.Data (I)
                    := F.Builtin_With_Meta.Builtin.all (Src (I .. I));
               end loop;
            when Kind_Fn =>
               for I in Src'Range loop
                  Ref.all.Data (I) := F.Fn.Apply (Src (I .. I));
               end loop;
            when others =>
               Err.Raise_With ("parameter 1 must be a function");
            end case;
         end return;
      end;
   end Map;

   function Meta (Item : in Ptr) return Mal.T
   is (Item.Ref.all.Meta);

   function Nth (Args : in Mal.T_Array) return Mal.T is
   begin
      Err.Check (Args'Length = 2, "expected 2 parameters");
      Err.Check (Args (Args'First).Kind in Kind_Sequence,
                 "paramater 1 must be a sequence");
      Err.Check (Args (Args'Last).Kind = Kind_Number,
                 "parameter 2 must be a number");
      declare
         L : Mal.T_Array renames Args (Args'First).Sequence.Ref.all.Data;
         I : constant Integer := Args (Args'Last).Number + 1;
      begin
         Err.Check (I in L'Range, "index out of bounds");
         return L (I);
      end;
   end Nth;

   function Rest (Args : in Mal.T_Array) return Mal.T is
   begin
      Err.Check (Args'Length = 1, "expected 1 parameter");
      declare
         A1  : Mal.T renames Args (Args'First);
         Ref : Acc;
      begin
         --  Avoid exceptions until Ref is controlled.
         case A1.Kind is
            when Kind_Nil =>
               Allocations := Allocations + 1;
               Ref := new Rec (0);
            when Kind_Sequence =>
               Allocations := Allocations + 1;
               if A1.Sequence.Ref.all.Last = 0 then
                  Ref := new Rec (0);
               else
                  Ref := new Rec'
                    (Last   => A1.Sequence.Ref.all.Last - 1,
                     Data   => A1.Sequence.Ref.all.Data
                                  (2 .. A1.Sequence.Ref.all.Data'Last),
                     others => <>);
               end if;
            when others =>
               Err.Raise_With ("parameter must be nil or a sequence");
         end case;
         return (Kind_List, (AFC with Ref));
      end;
   end Rest;

   function Tail (Source : in Ptr;
                  Count  : in Natural) return Mal.T_Array is
      Data : Mal.T_Array renames Source.Ref.all.Data;
   begin
      return Data (Data'Last - Count + 1 .. Data'Last);
   end Tail;

   function Vector (Args : in Mal.T_Array) return Mal.T is
   begin
      Allocations := Allocations + 1;
      return (Kind_Vector, (AFC with new Rec'(Data   => Args,
                                              Last   => Args'Length,
                                              others => <>)));
   end Vector;

   function With_Meta (Data     : in Ptr;
                       Metadata : in Mal.T) return Ptr
   is
      --  Avoid exceptions until Ref is controlled.
      Ref : Acc := Data.Ref;

   begin
      pragma Assert (0 < Ref.all.Refs);
      if Ref.all.Refs = 1 then
         Ref.all.Refs := 2;
         Ref.all.Meta := Metadata;
      else
         Allocations := Allocations + 1;
         Ref := new Rec'(Last   => Ref.all.Last,
                         Data   => Ref.all.Data,
                         Meta   => Metadata,
                         others => <>);
      end if;
      return (AFC with Ref);
   end With_Meta;

end Types.Sequences;
