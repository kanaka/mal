with Garbage_Collected;

package Types.Sequences is

   --  Hiding the implementation would either cause a significative
   --  performance hit (the compiler performs better optimization with
   --  explicit arrays) or a convoluted interface (demonstrated for
   --  strings and maps, where the balance is different).

   type Instance (Length : Natural) is new Garbage_Collected.Instance with
      record
         Meta : T;
         Data : T_Array (1 .. Length);
      end record;

   --  Built-in functions.
   function Concat   (Args : in T_Array) return T;
   function Conj     (Args : in T_Array) return T;
   function Cons     (Args : in T_Array) return T;
   function Count    (Args : in T_Array) return T;
   function First    (Args : in T_Array) return T;
   function Is_Empty (Args : in T_Array) return T;
   function List     (Args : in T_Array) return T;
   function Map      (Args : in T_Array) return T;
   function Nth      (Args : in T_Array) return T;
   function Rest     (Args : in T_Array) return T;
   function Vector   (Args : in T_Array) return T;

   --  New instances must be created via this constructor.
   function Constructor (Length : in Natural) return Sequence_Ptr with Inline;

   --  Helper for Types."=".
   function "=" (Left, Right : in Instance) return Boolean;

private

   overriding procedure Keep_References (Object : in out Instance) with Inline;

end Types.Sequences;
