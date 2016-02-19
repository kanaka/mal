with Ada.Containers.Hashed_Maps;
with Ada.Strings.Unbounded.Hash;
with Smart_Pointers;
limited with Types;

package Envs is

   type Env_Handle is private;

   function New_Env (Outer : Env_Handle) return Env_Handle;

   -- Set adds an element to the environment E.
   procedure Set
     (E : Env_Handle;
      Key : String;
      Elem : Smart_Pointers.Smart_Pointer);

   -- Get finds a key in the E env.  If it can't be found it looks
   -- in an outer env.  If it runs out of envs, Not Found is raised.
   function Get (E : Env_Handle; Key : String)
   return Smart_Pointers.Smart_Pointer;

   procedure Set_Outer
     (E : Env_Handle; Outer_Env : Env_Handle);

   function To_String (E : Env_Handle) return String;

   Not_Found : exception;

   -- Sym and Exprs are lists.  Bind Sets Keys in Syms to the corresponding
   -- expression in Exprs.  Returns true if all the parameters were bound.
   function Bind (E : Env_Handle; Syms, Exprs : Types.List_Mal_Type)
   return Boolean;

   -- Create a New_Env. The previous one is pushed to the stack and the
   -- new one becomes the current one.
   procedure New_Env;

   -- Destroys the top-most env and replaces it with the previous one
   -- in the stack.
   procedure Delete_Env;

   function Get_Current return Env_Handle;

   Debug : Boolean := False;

private

   type Env_Handle is new Smart_Pointers.Smart_Pointer;

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

   Current : Env_Handle;

   type Env_Ptr is access all Env;

   function Deref (SP : Env_Handle) return Env_Ptr;

end Envs;
