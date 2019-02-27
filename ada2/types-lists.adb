with Ada.Unchecked_Deallocation;

with Types.Mal;

package body Types.Lists is

   subtype AFC is Ada.Finalization.Controlled;
   use type Mal.T_Array;

   type Rec (Last : Natural) is limited record
      Refs : Natural                 := 1;
      Meta : Mal.T                   := Mal.Nil;
      Data : Mal.T_Array (1 .. Last) := (others => Mal.Nil);
   end record;

   procedure Free is new Ada.Unchecked_Deallocation (Rec, Acc);

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
      Object.Ref.all.Refs := Object.Ref.all.Refs + 1;
   end Adjust;

   function Concat (Args : in Mal.T_Array) return Mal.T is
      Sum : Natural := 0;
      Ref : Acc;
   begin
      for Arg of Args loop
         if Arg.Kind not in Kind_List | Kind_Vector then
            raise Argument_Error with "concat: expects lists or vectors";
         end if;
         Sum := Sum + Arg.L.Ref.all.Last;
      end loop;
      Ref := new Rec (Sum);
      for Arg of reverse Args loop
         Ref.all.Data (Sum - Arg.L.Ref.all.Last + 1 .. Sum)
           := Arg.L.Ref.all.Data;
         Sum := Sum - Arg.L.Ref.all.Last;
      end loop;
      pragma Assert (Sum = 0);
      return (Kind_List, (AFC with Ref));
   end Concat;

   function Conj (Args : in Mal.T_Array) return Mal.T is
      Ref : Acc;
   begin
      if Args'Length = 0 then
         raise Argument_Error with "conj: expects at least 1 argument";
      end if;
      case Args (Args'First).Kind is
         when Kind_List =>
            Ref := new Rec
              (Args'Length - 1 + Args (Args'First).L.Ref.all.Last);
            Ref.all.Data (Args'Length .. Ref.all.Last)
              := Args (Args'First).L.Ref.all.Data;
            for I in 1 .. Args'Length - 1 loop
               Ref.all.Data (I) := Args (Args'Last - I + 1);
            end loop;
            return (Kind_List, (AFC with Ref));
         when Kind_Vector =>
            return (Kind_Vector, (AFC with new Rec'
               (Last   => Args'Length - 1 + Args (Args'First).L.Ref.all.Last,
                Data   => Args (Args'First).L.Ref.all.Data
                  & Args (Args'First + 1 .. Args'Last),
                others => <>)));
         when others =>
            raise Argument_Error with "conj: first arg must be list or vector";
      end case;
   end Conj;

   function Cons (Args : in Mal.T_Array) return Mal.T is
   begin
      if Args'Length /= 2 then
         raise Argument_Error with "cons: expects 2 arguments";
      end if;
      case Args (Args'Last).Kind is
         when Kind_List | Kind_Vector =>
            return (Kind_List, (AFC with new Rec'
               (Last   => 1 + Args (Args'Last).L.Ref.all.Last,
                Data   => Args (Args'First) & Args (Args'Last).L.Ref.all.Data,
                others => <>)));
         when others =>
            raise Argument_Error with "cons: last arg must be list or vector";
      end case;
   end Cons;

   function Count (Args : in Mal.T_Array) return Mal.T
   is (if Args'Length /= 1 then
          raise Argument_Error with "count: expects 1 argument"
       else
         (case Args (Args'First).Kind is
            when Kind_Nil =>
               (Kind_Number, 0),
            when Kind_List | Kind_Vector =>
               (Kind_Number, Args (Args'First).L.Ref.all.Last),
            when others =>
               raise Argument_Error with "count: expects a list or vector"));

   function Element (Container : in Ptr;
                     Index     : in Positive) return Mal.T
   is (Container.Ref.all.Data (Index));

   procedure Finalize (Object : in out Ptr) is
   begin
      if Object.Ref /= null and then 0 < Object.Ref.all.Refs then
         Object.Ref.all.Refs := Object.Ref.all.Refs - 1;
         if 0 < Object.Ref.all.Refs then
            Object.Ref := null;
         else
            Free (Object.Ref);
         end if;
      end if;
   end Finalize;

   function First (Args : in Mal.T_Array) return Mal.T
   is (if Args'Length /= 1 then
          raise Argument_Error with "first: expects 1 argument"
       else
          (case Args (Args'First).Kind is
             when Kind_Nil =>
                Mal.Nil,
             when Kind_List | Kind_Vector =>
                (if Args (Args'First).L.Ref.all.Last = 0 then
                    Mal.Nil
                 else
                    Args (Args'First).L.Ref.all.Data (1)),
             when others =>
                raise Argument_Error with "first: expects a list or vector"));

   function Generic_Eval (Container : in Ptr;
                          Env       : in Env_Type)
                         return Ptr is
      --  Take care that automatic deallocation happens if an
      --  exception is propagated by user code.
      Old : Rec renames Container.Ref.all;
      Ref : Acc;
   begin
      pragma Assert (0 < Old.Refs);
      if Old.Refs = 1 then
         Ref := Container.Ref;
         Old.Refs := 2;
         Old.Meta := Mal.Nil;
      else
         Ref := new Rec (Old.Last);
      end if;
      return R : constant Ptr := (AFC with Ref) do
         for I in Old.Data'Range loop
            Ref.all.Data (I) := Eval (Old.Data (I), Env);
         end loop;
      end return;
   end Generic_Eval;

   function Is_Empty (Args : in Mal.T_Array) return Mal.T
   is (if Args'Length /= 1 then
          raise Argument_Error with "empty?: expects 1 argument"
       else
          (case Args (Args'First).Kind is
             when Kind_List | Kind_Vector =>
                (Kind_Boolean, Args (Args'First).L.Ref.all.Last = 0),
             when others =>
                raise Argument_Error with "empty?: expects a list or vector"));

   function Length (Source : in Ptr) return Natural
   is (Source.Ref.all.Last);

   function List (Args : in Mal.T_Array) return Mal.T
   is (Kind_List, (AFC with new Rec'(Data   => Args,
                                     Last   => Args'Length,
                                     others => <>)));

   function Meta (Item : in Ptr) return Mal.T
   is (Item.Ref.all.Meta);

   function Nth (Args : in Mal.T_Array) return Mal.T
   is (if Args'Length /= 2 then
          raise Argument_Error with "nth: expects 2 arguments"
       else
          (case Args (Args'First).Kind is
             when Kind_List | Kind_Vector =>
                (if Args (Args'First + 1).Kind /= Kind_Number then
                  raise Argument_Error with "nth: last arg must be a number"
                elsif 1 + Args (Args'Last).Ada_Number
                      in Args (Args'First).L.Ref.all.Data'Range
                then
                   Args (Args'First).L.Ref.all.Data
                      (1 + Args (Args'Last).Ada_Number)
                else
                   raise Argument_Error with "nth: index out of bounds"),
             when others =>
                   raise Argument_Error with "nth: expects a list or vector"));

   function Rest (Args : in Mal.T_Array) return Mal.T
   is (Kind_List, (AFC with
      (if Args'Length /= 1 then
          raise Argument_Error with "rest: expects 1 argument"
       else
          (case Args (Args'First).Kind is
             when Kind_Nil =>
                new Rec (0),
             when Kind_List | Kind_Vector =>
                (if Args (Args'First).L.Ref.all.Last = 0 then
                   new Rec (0)
                else
                   new Rec'(Last => Args (Args'First).L.Ref.all.Last - 1,
                            Data => Args (Args'First).L.Ref.all.Data
                                       (2 .. Args (Args'First).L.Ref.all.Last),
                            others => <>)),
             when others =>
                raise Argument_Error with "rest: expects a list or vector"))));

   function Slice (Item  : in Ptr;
                   Start : in Positive)
                  return Mal.T
   is (Kind_List, (AFC with new Rec'
      (Last   => Item.Ref.all.Last - Start + 1,
       Data   => Item.Ref.all.Data (Start .. Item.Ref.all.Last),
       others => <>)));

   function Vector (Args : in Mal.T_Array) return Mal.T
   is (Kind_Vector, (AFC with new Rec'(Data   => Args,
                                       Last   => Args'Length,
                                       others => <>)));

   function With_Meta (Data : in Ptr;
                       Meta : in Mal.T)
                      return Ptr is
      Old : Rec renames Data.Ref.all;
      Ref : Acc;
   begin
      pragma Assert (0 < Old.Refs);
      if Old.Refs = 1 then
         Ref := Data.Ref;
         Old.Refs := 2;
         Old.Meta := Meta;
      else
         Ref := new Rec'(Last => Old.Last,
                         Data => Old.Data,
                         Meta => Meta,
                         others => <>);
      end if;
      return (AFC with Ref);
   end With_Meta;

end Types.Lists;
