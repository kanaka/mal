private with Ada.Finalization;

with Types.Lists;
with Types.Mal;
with Types.Symbols;

package Envs with Elaborate_Body is

   --  This package should be named Env, but Ada does not allow formal
   --  parameters to be named like a package dependency, and it seems
   --  that readability inside Eval is more important.

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
   --  Allows assignment to a freshly created variable.  This is
   --  required for tail call optimization, but should be avoided
   --  elsewhere.

   procedure Replace_With_Sub (Env : in out Ptr) with Inline;
   --  Equivalent to Env := Sub (Outer => Env, empty Binds and Exprs),
   --  except that such an assignment is forbidden for performance
   --  reasons.

   procedure Set (Env         : in Ptr;
                  Key         : in Types.Symbols.Ptr;
                  New_Element : in Types.Mal.T)
     with Inline;

   --  The Find method is merged into the Get method.

   function Get (Evt : in Ptr;
                 Key : in Types.Symbols.Ptr) return Types.Mal.T;
   Unknown_Key : exception;

   --  Function closures.

   type Closure_Ptr is tagged private;
   Null_Closure : constant Closure_Ptr;

   function New_Closure (Env : in Ptr'Class) return Closure_Ptr;
   --  The class-wide argument does not make much sense, but avoids
   --  the compiler wondering on which type is should dispatch.

   function Sub (Outer : in Closure_Ptr'Class;
                 Binds : in Types.Symbols.Symbol_Array;
                 Exprs : in Types.Mal.T_Array) return Ptr;
   --  Construct a new environment with the given closure as outer parent.
   --  Then call Set with the paired elements of Binds and Exprs,
   --  handling the "&" special formal parameter if present.
   --  May raise Argument_Count.

   procedure Replace_With_Sub (Env   : in out Ptr;
                               Outer : in     Closure_Ptr'Class;
                               Binds : in     Types.Symbols.Symbol_Array;
                               Exprs : in     Types.Mal.T_Array);
   --  Equivalent to Env := Sub (Outer, Binds, Expr); except that such
   --  an assignment is forbidden for performance reasons.

   function Sub (Outer : in Ptr;
                 Binds : in Types.Symbols.Symbol_Array;
                 Exprs : in Types.Lists.Ptr) return Ptr;
   --  Like Sub above, but dedicated to macros.
   --  * The Outer parameter is the current environment, not a closure.
   --  * The Exprs argument is a list.
   --  * Its first element is skipped.

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

end Envs;
