with Ada.Strings.Unbounded.Hash;
with Smart_Pointers;

package body Types.Hash_Map is

   function "=" (A, B : Hash_Map_Mal_Type) return Boolean is
      A_Key, A_Elem, B_Elem : Mal_Handle;
      use Mal_Mal_Hash;
      C : Cursor;
   begin
      if A.Length /= B.Length then
         return False;
      end if;
      C := A.Hash.First;
      while Has_Element (C) loop
         A_Key := Key (C);
         A_Elem := Element (C);
         B_Elem := Mal_Mal_Hash.Element (B.Hash, A_Key);
         if A_Elem /= B_Elem then
            return False;
         end if;
         Next (C);
      end loop;
      return True;
   end "=";

   function New_Hash_Map_Mal_Type
   return Mal_Handle is
   begin
      return Smart_Pointers.New_Ptr
        (new Hash_Map_Mal_Type'
           (Mal_Type with
              List_Type => Hashed_List,
              The_List => Smart_Pointers.Null_Smart_Pointer,
              Last_Elem => Smart_Pointers.Null_Smart_Pointer,
              Is_Key_Expected => True,
              Next_Key => Smart_Pointers.Null_Smart_Pointer,
              Hash => Mal_Mal_Hash.Empty_Map));
   end New_Hash_Map_Mal_Type;

   overriding function Prepend (Op : Mal_Handle; To_Vector : Hash_Map_Mal_Type)
   return Mal_Handle is
   begin
      raise Not_Appropriate;
      return Smart_Pointers.Null_Smart_Pointer;
   end Prepend;

   overriding procedure Append (V : in out Hash_Map_Mal_Type; E : Mal_Handle) is
   begin
      if V.Is_Key_Expected then
         V.Next_Key := E;
      else
         Mal_Mal_Hash.Include
           (Container => V.Hash,
            Key => V.Next_Key,
            New_Item => E);
      end if;
      V.Is_Key_Expected := not V.Is_Key_Expected;
   end Append;

   overriding function Length (L : Hash_Map_Mal_Type) return Natural is
   begin
      return Natural (L.Hash.Length);
   end Length;

   overriding function Is_Null (L : Hash_Map_Mal_Type) return Boolean is
   begin
      return L.Hash.Is_Empty;
   end Is_Null;

   overriding function Null_List (L : List_Types) return Hash_Map_Mal_Type is
   begin
      return 
        Hash_Map_Mal_Type'
           (Mal_Type with
              List_Type => Hashed_List,
              The_List => Smart_Pointers.Null_Smart_Pointer,
              Last_Elem => Smart_Pointers.Null_Smart_Pointer,
              Is_Key_Expected => False,
              Next_Key => Smart_Pointers.Null_Smart_Pointer,
              Hash => Mal_Mal_Hash.Empty_Map);
   end Null_List;

   -- Duplicate copies the list (logically).  This is to allow concatenation,
   -- The result is always a List_List.
   overriding function Duplicate (The_List : Hash_Map_Mal_Type) return Mal_Handle is
   begin
      raise Not_Appropriate;
      return Smart_Pointers.Null_Smart_Pointer;
   end Duplicate;

   overriding function Nth (L :Hash_Map_Mal_Type; N : Natural) return Mal_Handle is
   begin
      raise Not_Appropriate;
      return Smart_Pointers.Null_Smart_Pointer;
   end Nth;

   overriding procedure Add_Defs (Defs : Hash_Map_Mal_Type; Env : Envs.Env_Handle) is
   begin
      raise Not_Appropriate;
   end Add_Defs;

   -- Get the first item in the list:
   overriding function Car (L : Hash_Map_Mal_Type) return Mal_Handle is
   begin
      raise Not_Appropriate;
      return Smart_Pointers.Null_Smart_Pointer;
   end Car;

   -- Get the rest of the list (second item onwards)
   overriding function Cdr (L : Hash_Map_Mal_Type) return Mal_Handle is
   begin
      raise Not_Appropriate;
      return Smart_Pointers.Null_Smart_Pointer;
   end Cdr;


   overriding function Map
     (Func_Ptr : Func_Access;
      L : Hash_Map_Mal_Type)
   return Mal_Handle is
      Res : Mal_Handle;
      use Mal_Mal_Hash;
      C : Cursor;
   begin
      Res := New_Hash_Map_Mal_Type;
      C := L.Hash.First;
      while Has_Element (C) loop
         -- Assuming we're not applying the func to the keys too.
         Deref_Hash (Res).Hash.Include
           (Key => Key (C),
            New_Item => Func_Ptr (Element (C)));
         Next (C);
      end loop;
      return Res;
   end Map;

   function Assoc (H : Hash_Map_Mal_Type; List : Mal_Handle) return Mal_Handle is
      Res : Mal_Handle;
      Rest_List : List_Mal_Type;
      use Mal_Mal_Hash;
      C : Cursor;
   begin
      Res := New_Hash_Map_Mal_Type;
      Rest_List := Deref_List (List).all;

      -- Copy arg into result.
      Deref_Hash (Res).Hash := H.Hash;
         
      while not Is_Null (Rest_List) loop
         Deref_Hash (Res).Append (Car (Rest_List));
         Rest_List := Deref_List (Cdr (Rest_List)).all;
      end loop;
      return Res;
   end Assoc;


   function Dis_Assoc (H : Hash_Map_Mal_Type; List : Mal_Handle) return Mal_Handle is
      Res : Mal_Handle;
      Rest_List : List_Mal_Type;
      use Mal_Mal_Hash;
      C : Cursor;
   begin
      Res := New_Hash_Map_Mal_Type;
      Rest_List := Deref_List (List).all;

      -- Copy arg into result.
      Deref_Hash (Res).Hash := H.Hash;
         
      while not Is_Null (Rest_List) loop
         Mal_Mal_Hash.Exclude (Deref_Hash (Res).Hash, Car (Rest_List));
         Rest_List := Deref_List (Cdr (Rest_List)).all;
      end loop;
      return Res;
   end Dis_Assoc;


   function Get (H : Hash_Map_Mal_Type; Key : Mal_Handle) return Mal_Handle is
      use Mal_Mal_Hash;
      C : Cursor;
   begin
      C := Mal_Mal_Hash.Find (H.Hash, Key);
      if Has_Element (C) then
         return Element (C);
      else
         return New_Nil_Mal_Type;
      end if;
   end Get;


   function All_Keys (H : Hash_Map_Mal_Type) return Mal_Handle is
      Res, Map_Key : Mal_Handle;
      use Mal_Mal_Hash;
      C : Cursor;
   begin
      Res := New_List_Mal_Type (List_List);
      C := H.Hash.First;
      while Has_Element (C) loop
         Map_Key := Key (C);
         Deref_List (Res).Append (Map_Key);
         Next (C);
      end loop;
      return Res;
   end All_Keys;


   function All_Values (H : Hash_Map_Mal_Type) return Mal_Handle is
      Res, Map_Val : Mal_Handle;
      use Mal_Mal_Hash;
      C : Cursor;
   begin
      Res := New_List_Mal_Type (List_List);
      C := H.Hash.First;
      while Has_Element (C) loop
         Map_Val := Element (C);
         Deref_List (Res).Append (Map_Val);
         Next (C);
      end loop;
      return Res;
   end All_Values;


   function Contains (H : Hash_Map_Mal_Type; Key : Mal_Handle) return Boolean is
   begin
      return Mal_Mal_Hash.Contains (H.Hash, Key);
   end Contains;

   function Deref_Hash (SP : Mal_Handle) return Hash_Ptr is
   begin
      return Hash_Ptr (Deref (SP));
   end Deref_Hash;

   function Hash (M : Mal_Handle) return Ada.Containers.Hash_Type is
   begin
      return Ada.Strings.Unbounded.Hash
               (Ada.Strings.Unbounded.To_Unbounded_String
                 (Deref (M).To_String));
   end Hash;

   overriding function To_Str 
     (T : Hash_Map_Mal_Type; Print_Readably : Boolean := True)
   return Mal_String is
     use Ada.Containers;
   begin
      if (T.Hash.Length = 0) then
         return Opening (T.List_Type) &
                Closing (T.List_Type);
      else
         declare
            Res : Ada.Strings.Unbounded.Unbounded_String;
            use Mal_Mal_Hash;
            C : Cursor;
         begin
            C := First (T.Hash);
 
            Res := Ada.Strings.Unbounded."&"
                     (Opening (T.List_Type),
                      Ada.Strings.Unbounded.To_Unbounded_String
                        (To_String (Deref (Key (C)).all, Print_Readably)));
            Res := Ada.Strings.Unbounded."&" (Res, " ");
            Res := Ada.Strings.Unbounded."&"
                     (Res,
                      Ada.Strings.Unbounded.To_Unbounded_String
                        (To_String (Deref (Element (C)).all, Print_Readably)));
            Next (C);
            while Has_Element (C) loop
               Res := Ada.Strings.Unbounded."&" (Res, " ");
               Res := Ada.Strings.Unbounded."&"
                        (Res,
                         Ada.Strings.Unbounded.To_Unbounded_String
                           (To_String (Deref (Key (C)).all, Print_Readably)));
               Res := Ada.Strings.Unbounded."&" (Res, " ");
               Res := Ada.Strings.Unbounded."&"
                 (Res,
                  Ada.Strings.Unbounded.To_Unbounded_String
                    (To_String (Deref (Element (C)).all, Print_Readably)));
               Next (C);
            end loop;
            Res := Ada.Strings.Unbounded."&" (Res, Closing (T.List_Type));
            return Ada.Strings.Unbounded.To_String (Res);
         end;
      end if;
   end To_Str;

end Types.Hash_Map;
