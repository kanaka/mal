private with Ada.Finalization;

limited with Types.Mal;

package Types.Maps is

   type Ptr is tagged private;

   --  Built-in functions.
   function Assoc    (Args : in Mal.T_Array) return Mal.T;
   function Contains (Args : in Mal.T_Array) return Mal.T;
   function Dissoc   (Args : in Mal.T_Array) return Mal.T;
   function Get      (Args : in Mal.T_Array) return Mal.T;
   function Hash_Map (Args : in Mal.T_Array) return Mal.T;
   function Keys     (Args : in Mal.T_Array) return Mal.T;
   function Vals     (Args : in Mal.T_Array) return Mal.T;

   --  A generic is better than an access to function because of
   --  https://gcc.gnu.org/bugzilla/show_bug.cgi?id=89159

   --  Used to evaluate each element of a map.
   --  Eval is generic because units cannot depend on each other.
   generic
      type Env_Type (<>) is limited private;
      with function Eval (Ast : in Mal.T;
                          Env : in Env_Type)
                         return Mal.T;
   function Generic_Eval (Container : in Ptr;
                          Env       : in Env_Type)
                         return Mal.T;

   --  Used to print a map.
   generic
      with procedure Process (Key     : in Mal.T;
                              Element : in Mal.T);
   procedure Iterate (Container : in Ptr);

   function Meta (Container : in Ptr) return Mal.T with Inline;

   function With_Meta (Data     : in Ptr;
                       Metadata : in Mal.T)
                      return Mal.T;

   --  Debug
   procedure Check_Allocations;

private

   type Rec;
   type Acc is access Rec;
   type Ptr is new Ada.Finalization.Controlled with record
      Ref : Acc := null;
   end record
     with Invariant => Ptr.Ref /= null;
   overriding procedure Adjust   (Object : in out Ptr) with Inline;
   overriding procedure Finalize (Object : in out Ptr) with Inline;
   overriding function "=" (Left, Right : in Ptr) return Boolean with Inline;
   pragma Finalize_Storage_Only (Ptr);

end Types.Maps;
