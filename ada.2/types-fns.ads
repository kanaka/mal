private with Ada.Finalization;

limited with Envs;
limited with Types.Mal;
limited with Types.Sequences;
limited with Types.Symbols;

package Types.Fns is

   type Ptr is tagged private;
   --  A pointer to an user-defined function or macro.

   function New_Function (Params : in Sequences.Ptr;
                          Ast    : in Mal.T;
                          Env    : in Envs.Closure_Ptr) return Mal.T
     with Inline;
   --  Raise an exception if Params contains something else than symbols.

   function New_Macro (Item : in Ptr) return Mal.T with Inline;

   function Params (Item : in Ptr) return Symbols.Symbol_Array with Inline;
   function Ast (Item : in Ptr) return Mal.T with Inline;
   --  Useful to print.

   function Apply (Item : in Ptr;
                   Args : in Mal.T_Array) return Mal.T with Inline;
   --  Fails for macros.

   function Env (Item : in Ptr) return Envs.Closure_Ptr with Inline;
   --  Fails for macros. Required for TCO, instead of Apply.

   function Meta (Item : in Ptr) return Mal.T with Inline;
   --  Fails for macros.
   function With_Meta (Item     : in Ptr;
                       Metadata : in Mal.T) return Mal.T with Inline;
   --  Fails for macros.

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
   pragma Finalize_Storage_Only (Ptr);

end Types.Fns;
