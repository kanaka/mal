private with Ada.Containers.Hashed_Maps;

with Garbage_Collected;
with Types.Mal;

package Types.Maps is

   type Instance (<>) is new Garbage_Collected.Instance with private;

   --  Built-in functions.
   function Assoc    (Args : in Mal.T_Array) return Mal.T;
   function Contains (Args : in Mal.T_Array) return Mal.T;
   function Dissoc   (Args : in Mal.T_Array) return Mal.T;
   function Get      (Args : in Mal.T_Array) return Mal.T;
   function Hash_Map (Args : in Mal.T_Array) return Mal.T;
   function Keys     (Args : in Mal.T_Array) return Mal.T;
   function Vals     (Args : in Mal.T_Array) return Mal.T;

   function "=" (Left, Right : in Instance) return Boolean with Inline;

   --  A generic is better than an access to function because of
   --  https://gcc.gnu.org/bugzilla/show_bug.cgi?id=89159

   --  Used to evaluate each element of a map.

   generic
      type Env_Type (<>) is limited private;
      with function Eval (Ast : in Mal.T;
                          Env : in Env_Type)
                         return Mal.T;
   function Generic_Eval (Container : in Instance;
                          Env       : in Env_Type)
                         return Mal.T;

   --  Used to print a map.
   generic
      with procedure Process (Key     : in Mal.T;
                              Element : in Mal.T);
   procedure Iterate (Container : in Instance);

   function Length (Container : in Instance) return Natural with Inline;

   function Meta (Container : in Instance) return Mal.T with Inline;
   function With_Meta (Data     : in Instance;
                       Metadata : in Mal.T)
                      return Mal.T;

private

   function Hash (Item : in Mal.T) return Ada.Containers.Hash_Type with Inline;
   --  This function also checks the kind of the key, and raise an
   --  error in case of problem.

   package HM is new Ada.Containers.Hashed_Maps (Key_Type        => Mal.T,
                                                 Element_Type    => Mal.T,
                                                 Hash            => Hash,
                                                 Equivalent_Keys => Mal."=",
                                                 "="             => Mal."=");
   use type HM.Map;

   type Instance is new Garbage_Collected.Instance with record
      Data   : HM.Map;
      F_Meta : Mal.T;
   end record;
   overriding procedure Keep_References (Object : in out Instance) with Inline;

end Types.Maps;
