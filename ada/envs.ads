with Ada.Containers.Hashed_Maps;
with Ada.Strings.Unbounded;
with Smart_Pointers;

package Envs is

   procedure Set (Key : String; SP : Smart_Pointers.Smart_Pointer);

   function Get (Key : String) return Smart_Pointers.Smart_Pointer;

   -- Create a New_Env. THe previous one is pushed to the stack and the
   -- new one becomes the current one.
   procedure New_Env;

   -- Destroys the top-most env and replaces it with the previous one
   -- in the stack.
   procedure Delete_Env;

private

   function String_Hash (Key : Ada.Strings.Unbounded.Unbounded_String)
   return Ada.Containers.Hash_Type;

   package String_Mal_Hash is new Ada.Containers.Hashed_Maps
     (Key_Type        => Ada.Strings.Unbounded.Unbounded_String,
      Element_Type    => Smart_Pointers.Smart_Pointer,
      Hash            => String_Hash,
      Equivalent_Keys => Ada.Strings.Unbounded."=",
      "="             => Smart_Pointers."=");

   type Environment is record
      The_Map : String_Mal_Hash.Map;
   end record;

   type Env_Ptr is access Environment;

   Current : Env_Ptr;

end Envs;
