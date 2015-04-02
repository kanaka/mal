with Unchecked_Deallocation;

package body Envs is


   procedure Set (Key : String; SP : Smart_Pointers.Smart_Pointer) is
   begin
      String_Mal_Hash.Include
        (Container => Current.The_Map,
         Key       => Ada.Strings.Unbounded.To_Unbounded_String (Key),
         New_Item  => SP);
   end Set;


   function Get (Key : String) return Smart_Pointers.Smart_Pointer is
      use String_Mal_Hash;
      C : Cursor;
   begin
      C := Find (Current.The_Map,
                 Ada.Strings.Unbounded.To_Unbounded_String (Key));
      return Element (C);
   end Get;


   function String_Hash (Key : Ada.Strings.Unbounded.Unbounded_String)
   return Ada.Containers.Hash_Type is

      use Ada.Containers;
      Res : Ada.Containers.Hash_Type;
      Str_Len : Natural;
   begin
      Res := 0;
      Str_Len := Ada.Strings.Unbounded.Length (Key);
      for I in 1..Str_Len loop
         Res := Res * 16 +
                  Character'Pos (Ada.Strings.Unbounded.Element (Key, I));
      end loop;
      return Res;
   end String_Hash;


   procedure New_Env is
   begin
      Current := new Environment;
   end New_Env;


   procedure Free is new Unchecked_Deallocation (Environment, Env_Ptr);

   procedure Delete_Env is
   begin
      Free (Current);
   end Delete_Env;
   

end Envs;
