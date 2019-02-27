private with Ada.Finalization;

with Types.Mal;
with Types.Symbols;

package Environments with Elaborate_Body is

   --  This implementation relies on the fact that the caller only
   --  ever references environments in its execution stack.

   --  When a function closure references an environment that the
   --  execution leaves behind, a dynamically allocated block is used
   --  instead.

   --  The eval built-in requires REPL (see the implementation of
   --  load-file), so we cannot assume that the caller only sees the
   --  current environment.

   type Ptr (<>) is tagged limited private;
   --  This type is controlled in order count the references to a
   --  given environment, even during exception propagation.
   --  Since Ptr is limited with a hidden discriminant, any variable
   --  must immediately be assigned with one of
   --  * Repl (in which case a renaming is probably better),
   --  * Copy_Pointer,
   --  * Sub (either from a Ptr or from a Closure_Ptr).
   --  Usual assignment with reference counting is not provided
   --  because we want to enforce the use of the more efficient
   --  Replace_With_Sub.

   Repl : constant Ptr;
   --  The top environment.

   function Copy_Pointer (Env : in Ptr) return Ptr with Inline;

   function Sub (Outer : in Ptr) return Ptr with Inline;

   procedure Replace_With_Sub (Env : in out Ptr) with Inline;
   --  Like Env := Sub (Outer => Env); except that Env is finalized
   --  *before* the assignement, so its memory may be reused by the
   --  new environment.

   procedure Set (Env         : in Ptr;
                  Key         : in Types.Symbols.Ptr;
                  New_Element : in Types.Mal.T)
     with Inline;

   function Get (Env : in Ptr;
                 Key : in Types.Symbols.Ptr)
                return Types.Mal.T;
   Unknown_Key : exception;

   --  Function closures.

   type Closure_Ptr is tagged private;
   Null_Closure : constant Closure_Ptr;

   function Sub (Outer : in Closure_Ptr'Class) return Ptr;

   procedure Replace_With_Sub (Env   : in out Ptr;
                               Outer : in     Closure_Ptr'Class);
   --  Like Env := Sub (Outer => Outer); except that Env is finalized
   --  *before* the assignement, so its memory can be reused by the
   --  new environment. This is important for tail call optimization.

   function New_Closure (Env : in Ptr'Class) return Closure_Ptr;
   --  The class-wide argument does not make much sense, but avoids
   --  the compiler wondering on which type is should dispatch.

private

   --  There must be a reference level so that functions may keep
   --  track of their initial environment, and another one for
   --  reallocations.  The second one is delegated to a predefined Ada
   --  container.

   --  MAL maps may be tempting, but we do not want to copy the whole
   --  map for each addition or removal.

   --  Some tests seem to show that a hashmap is three times faster
   --  than a vector with (key, value) couples.

   --  We allow the null value so that the empty environment in a
   --  macro does not trigger an allocation.

   --      300 for normal tests
   --    7_500 for make ada2 && make MAL_IMPL=ada2 test^mal
   --  150_000 for make ada2 && make perf^ada2
   type Stack_Index is range 0 .. 150_000;

   --  See README for the implementation of reference counting.

   type Ptr is new Ada.Finalization.Limited_Controlled with record
      Index : Stack_Index := 0;
   end record
     with Invariant => Index in 1 .. Top;
   overriding procedure Finalize (Object : in out Ptr) with Inline;
   pragma Finalize_Storage_Only (Ptr);

   Top : Stack_Index := 1;
   Repl : constant Ptr := (Ada.Finalization.Limited_Controlled with 1);

   type Heap_Record;
   type Heap_Access is access Heap_Record;
   type Closure_Ptr is new Ada.Finalization.Controlled with record
      Ref : Heap_Access := null;
   end record;
   overriding procedure Adjust   (Object : in out Closure_Ptr) with Inline;
   overriding procedure Finalize (Object : in out Closure_Ptr) with Inline;
   pragma Finalize_Storage_Only (Closure_Ptr);

   Null_Closure : constant Closure_Ptr
     := (Ada.Finalization.Controlled with null);

end Environments;
