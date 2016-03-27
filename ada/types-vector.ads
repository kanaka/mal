with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;
with Smart_Pointers;
with Envs;

package Types.Vector is

   type Vector_Mal_Type is new List_Mal_Type with private;

   function New_Vector_Mal_Type
   return Mal_Handle;

   overriding function Prepend (Op : Mal_Handle; To_Vector : Vector_Mal_Type)
   return Mal_Handle;

   overriding procedure Append (V : in out Vector_Mal_Type; E : Mal_Handle);

   overriding function Length (L : Vector_Mal_Type) return Natural;

   overriding function Is_Null (L : Vector_Mal_Type) return Boolean;

   overriding function Null_List (L : List_Types) return Vector_Mal_Type;

   -- Duplicate copies the list (logically).  This is to allow concatenation,
   -- The result is always a List_List.
   overriding function Duplicate (The_List : Vector_Mal_Type) return Mal_Handle;

   overriding function Nth (L : Vector_Mal_Type; N : Natural) return Mal_Handle;

   overriding procedure Add_Defs (Defs : Vector_Mal_Type; Env : Envs.Env_Handle);

   -- Get the first item in the list:
   overriding function Car (L : Vector_Mal_Type) return Mal_Handle;

   -- Get the rest of the list (second item onwards)
   overriding function Cdr (L : Vector_Mal_Type) return Mal_Handle;

   overriding function Map
     (Func_Ptr : Func_Access;
      L : Vector_Mal_Type)
   return Mal_Handle;

   type Vector_Ptr is access all Vector_Mal_Type;

   function Deref_Vector (SP : Mal_Handle) return Vector_Ptr;

private

   subtype Vec_Index is Integer range 0 .. 100;
   package Mal_Vectors is new
     Ada.Containers.Vectors
       (Index_Type => Vec_Index,
        Element_Type => Mal_Handle,
        "=" => "=");

   use Mal_Vectors;

   type Vector_Mal_Type is new List_Mal_Type with record
      Vec : Mal_Vectors.Vector;
   end record;

   overriding function To_Str 
     (T : Vector_Mal_Type; Print_Readably : Boolean := True)
   return Mal_String;

end Types.Vector;
