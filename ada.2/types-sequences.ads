with Garbage_Collected;
with Types.Mal;

package Types.Sequences is

   type Instance (<>) is new Garbage_Collected.Instance with private
     with Constant_Indexing => Element;

   --  Built-in functions.
   function Concat   (Args : in Mal.T_Array) return Mal.T;
   function Conj     (Args : in Mal.T_Array) return Mal.T;
   function Cons     (Args : in Mal.T_Array) return Mal.T;
   function Count    (Args : in Mal.T_Array) return Mal.T;
   function First    (Args : in Mal.T_Array) return Mal.T;
   function Is_Empty (Args : in Mal.T_Array) return Mal.T;
   function List     (Args : in Mal.T_Array) return Mal.T;
   function Map      (Args : in Mal.T_Array) return Mal.T;
   function Nth      (Args : in Mal.T_Array) return Mal.T;
   function Rest     (Args : in Mal.T_Array) return Mal.T;
   function Vector   (Args : in Mal.T_Array) return Mal.T;

   function "=" (Left, Right : in Instance) return Boolean with Inline;

   function Length (Source : in Instance) return Natural with Inline;

   function Element (Container : in Instance;
                     Index     : in Positive) return Mal.T
     with Inline, Pre => Index <= Length (Container);

   function "&" (Left  : in Mal.T_Array;
                 Right : in Instance) return Mal.T_Array with Inline;
   --  Used to implement Core.Apply.

   function Constructor (Length : in Natural) return Mal.Sequence_Ptr
     with Inline;
   procedure Replace_Element (Container : in out Instance;
                              Index     : in     Positive;
                              New_Item  : in     Mal.T)
     with Inline, Pre => Index <= Length (Container);

   --  Used in macro implementation.
   function Tail (Source : in Instance;
                  Count  : in Natural) return Mal.T_Array
     with Inline, Pre => Count <= Length (Source);

   function Meta (Item : in Instance) return Mal.T with Inline;
   function With_Meta (Data     : in Instance;
                       Metadata : in Mal.T)
                      return Mal.Sequence_Ptr;

private

   type Instance (Last : Natural) is new Garbage_Collected.Instance with record
      F_Meta : Mal.T;
      Data   : Mal.T_Array (1 .. Last);
   end record;
   overriding procedure Keep_References (Object : in out Instance) with Inline;

end Types.Sequences;
