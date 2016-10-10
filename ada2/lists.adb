with Ada.Unchecked_Deallocation;
with Atoms;
with Types;

package body Lists is

   type List_Record (Last : Positive) is limited record
      Data : Types.Mal_Type_Array (1 .. Last);
      Refs : Positive;
   end record;
   --  The invariant for Ptr is:
   --    Ptr.Ref = null or else Ptr.First <= Ptr.Ref.all.Last
   --  but we cannot express this in the specification because the limited
   --  view on Types forbids to define List_Record there.

   procedure Free is new Ada.Unchecked_Deallocation (Object => List_Record,
                                                     Name   => List_Access);

   ----------------------------------------------------------------------

   function "=" (Left, Right : in Ptr) return Boolean is
      (if Left.Ref = null
       then Right.Ref = null
       else
          --  As strange as it may seem, this assertion fails when
          --  running "(= [(list)] (list []))".
          --  pragma Assert
          --    ((Left.Ref.all.Data (1) = Right.Ref.all.Data (1))
          --     =
          --     (Left.Ref.all.Data (1 .. 1) = Right.Ref.all.Data (1 .. 1)));
          --  This may be a compiler bug.
         Right.Ref /= null
         and then Left.Ref.all.Last = Right.Ref.all.Last
         and then (for all I in 1 .. Left.Ref.all.Last =>
                     Types."=" (Left.Ref.all.Data (I),
                                Right.Ref.all.Data (I))));

   procedure Adjust (Object : in out Ptr) is
   begin
      if Object.Ref /= null then
         Object.Ref.all.Refs := Object.Ref.all.Refs + 1;
      end if;
   end Adjust;

   function Element (Container : in Ptr;
                     Index     : in Positive) return Types.Mal_Type is
      (Container.Ref.all.Data (Index));

   procedure Finalize (Object : in out Ptr)
   is
      Refs : Positive;
   begin
      --  Ensure that we can be called twice in a row (7.6.1(24)).
      if Object.Ref /= null then
         Refs := Object.Ref.all.Refs;
         if 1 < Refs then
            Object.Ref.all.Refs := Refs - 1;
            Object.Ref := null;
         else
            --  pragma Assert (Ptr (Object.Ref.all.Id) = Object.Ref);
            --  Ptr (Object.Ref.all.Id) := null;
            Free (Object.Ref);
         end if;
      end if;
   end Finalize;

   function Length (Source : in Ptr) return Natural is
      (if Source.Ref = null then 0 else Source.Ref.all.Last);

   function Alloc (Source : in Types.Mal_Type_Array) return Ptr is
      (if Source'Length = 0
         then Empty_List
         else (Ada.Finalization.Controlled with
                 Ref => new List_Record'(Data => Source,
                                         Last => Source'Length,
                                         Refs => 1)));

   function Alloc (Length : in Natural) return Ptr is
      (if Length = 0
         then Empty_List
         else (Ada.Finalization.Controlled with
                 Ref => new List_Record'
                 (Data => (1 .. Length => (Types.Kind_Nil, Atoms.No_Element)),
                  Last => Length,
                  Refs => 1)));

   procedure Replace_Element (Source    : in Ptr;
                              Index     : in Positive;
                              New_Value : in Types.Mal_Type) is
   begin
      pragma Assert (Source.Ref.all.Refs = 1);
      Source.Ref.all.Data (Index) := New_Value;
   end Replace_Element;

end Lists;
