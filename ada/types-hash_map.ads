with Ada.Containers.Hashed_Maps;
with Smart_Pointers;
with Envs;

package Types.Hash_Map is

   type Hash_Map_Mal_Type is new List_Mal_Type with private;

   function New_Hash_Map_Mal_Type
   return Mal_Handle;

   function "=" (A, B : Hash_Map_Mal_Type) return Boolean;

   overriding function Prepend (Op : Mal_Handle; To_Vector : Hash_Map_Mal_Type)
   return Mal_Handle;

   overriding procedure Append (V : in out Hash_Map_Mal_Type; E : Mal_Handle);

   overriding function Length (L : Hash_Map_Mal_Type) return Natural;

   overriding function Is_Null (L : Hash_Map_Mal_Type) return Boolean;

   overriding function Null_List (L : List_Types) return Hash_Map_Mal_Type;

   -- Duplicate copies the list (logically).  This is to allow concatenation,
   -- The result is always a List_List.
   overriding function Duplicate (The_List : Hash_Map_Mal_Type) return Mal_Handle;

   overriding function Nth (L :Hash_Map_Mal_Type; N : Natural) return Mal_Handle;

   overriding procedure Add_Defs (Defs : Hash_Map_Mal_Type; Env : Envs.Env_Handle);

   -- Get the first item in the list:
   overriding function Car (L : Hash_Map_Mal_Type) return Mal_Handle;

   -- Get the rest of the list (second item onwards)
   overriding function Cdr (L : Hash_Map_Mal_Type) return Mal_Handle;

   overriding function Map
     (Func_Ptr : Func_Access;
      L : Hash_Map_Mal_Type)
   return Mal_Handle;

   function Assoc (H : Hash_Map_Mal_Type; List : Mal_Handle) return Mal_Handle;

   function Dis_Assoc (H : Hash_Map_Mal_Type; List : Mal_Handle) return Mal_Handle;

   function Get (H : Hash_Map_Mal_Type; Key : Mal_Handle) return Mal_Handle;

   function All_Keys (H : Hash_Map_Mal_Type) return Mal_Handle;

   function All_Values (H : Hash_Map_Mal_Type) return Mal_Handle;

   function Contains (H : Hash_Map_Mal_Type; Key : Mal_Handle) return Boolean;

   type Hash_Ptr is access all Hash_Map_Mal_Type;

   function Deref_Hash (SP : Mal_Handle) return Hash_Ptr;

   Not_Appropriate : exception;

private

   function Hash (M : Mal_Handle) return Ada.Containers.Hash_Type;

   package Mal_Mal_Hash is new Ada.Containers.Hashed_Maps
     (Key_Type        => Mal_Handle,
      Element_Type    => Mal_Handle,
      Hash            => Hash,
      Equivalent_Keys => "=",
      "="             => "=");

   type Hash_Map_Mal_Type is new List_Mal_Type with record
      Is_Key_Expected : Boolean := True;
      Next_Key : Mal_Handle;
      Hash : Mal_Mal_Hash.Map;
   end record;

   overriding function To_Str 
     (T : Hash_Map_Mal_Type; Print_Readably : Boolean := True)
   return Mal_String;

end Types.Hash_Map;
