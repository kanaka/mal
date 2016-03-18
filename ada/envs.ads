with Ada.Containers.Hashed_Maps;
with Ada.Strings.Unbounded.Hash;
with Smart_Pointers;
limited with Types;

package Envs is

   type Env_Handle is private;

   Null_Env_Handle : constant Env_Handle;

   function New_Env (Outer : Env_Handle := Null_Env_Handle) return Env_Handle;

   -- Set adds an element to the environment E.
   procedure Set
     (E : Env_Handle;
      Key : String;
      Elem : Smart_Pointers.Smart_Pointer);

   -- Get finds a key in the E env.  If it can't be found it looks
   -- in an outer env.  If it runs out of envs, Not Found is raised.
   function Get (E : Env_Handle; Key : String) return Smart_Pointers.Smart_Pointer;

   Not_Found : exception;

   procedure Set_Outer
     (E : Env_Handle; Outer_Env : Env_Handle);

   -- Sym and Exprs are lists.  Bind Sets Keys in Syms to the corresponding
   -- expression in Exprs.  Returns true if all the parameters were bound.
   function Bind (Env : Env_Handle; Syms, Exprs : Types.List_Mal_Type)
   return Boolean;

   function To_String (E : Env_Handle) return String;

   Debug : Boolean := False;

private

   type Env_Handle is new Smart_Pointers.Smart_Pointer;

   Null_Env_Handle : constant Env_Handle :=
     Env_Handle (Smart_Pointers.Null_Smart_Pointer);

   function Is_Null (E : Env_Handle) return Boolean;

   package String_Mal_Hash is new Ada.Containers.Hashed_Maps
     (Key_Type        => Ada.Strings.Unbounded.Unbounded_String,
      Element_Type    => Smart_Pointers.Smart_Pointer,
      Hash            => Ada.Strings.Unbounded.Hash,
      Equivalent_Keys => Ada.Strings.Unbounded."=",
      "="             => Smart_Pointers."=");

   type Env is new Smart_Pointers.Base_Class with record
      The_Map : String_Mal_Hash.Map;
      Outer_Env : Env_Handle;
      Level: Natural;
   end record;

   type Env_Ptr is access all Env;

   function Deref (SP : Env_Handle) return Env_Ptr;

end Envs;
