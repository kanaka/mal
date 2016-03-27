with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Eval_Callback;

package body Types.Vector is


   function New_Vector_Mal_Type
   return Mal_Handle is
   begin
      return Smart_Pointers.New_Ptr
        (new Vector_Mal_Type'
           (Mal_Type with
              List_Type => Vector_List,
              The_List => Smart_Pointers.Null_Smart_Pointer,
              Last_Elem => Smart_Pointers.Null_Smart_Pointer,
              Vec => Mal_Vectors.Empty_Vector));
   end New_Vector_Mal_Type;


   overriding function Prepend (Op : Mal_Handle; To_Vector : Vector_Mal_Type)
   return Mal_Handle is
   begin
      return Types.Prepend (Op, Deref_List (To_Vector.Duplicate).all);
   end Prepend;


   overriding procedure Append (V : in out Vector_Mal_Type; E : Mal_Handle) is
   begin
      Mal_Vectors.Append (V.Vec, E);
   end Append;


   overriding function Is_Null (L : Vector_Mal_Type) return Boolean is
     use Ada.Containers;
   begin
      return L.Vec.Is_Empty;
   end Is_Null;


   overriding function Null_List (L : List_Types) return Vector_Mal_Type is
   begin
      return Vector_Mal_Type'
               (Mal_Type with
                  List_Type => Vector_List,
                  The_List => Smart_Pointers.Null_Smart_Pointer,
                  Last_Elem => Smart_Pointers.Null_Smart_Pointer,
                  Vec => Mal_Vectors.Empty_Vector);
   end Null_List;


   -- Duplicate copies the list (logically).  This is to allow concatenation,
   -- The result is always a List_List.
   overriding function Duplicate (The_List : Vector_Mal_Type) return Mal_Handle is
      Res : Mal_Handle;
      use Mal_Vectors;
      C : Cursor;
   begin
      Res := New_List_Mal_Type (List_List);
      C := First (The_List.Vec);
      while Has_Element (C) loop
         Deref_List (Res).Append (Element (C));
         Next (C);
      end loop;
      return Res;
   end Duplicate;
      

   function Length (L : Vector_Mal_Type) return Natural is
   begin
      return Natural (L.Vec.Length);
   end Length;


   procedure Add_Defs (Defs : Vector_Mal_Type; Env : Envs.Env_Handle) is
      C, D : Cursor;
   begin
      C := Defs.Vec.First;
      while Has_Element (C) loop
         D := Next (C);
         exit when not Has_Element (D);
         Envs.Set
           (Env,
            Deref_Sym (Element (C)).Get_Sym,
            Eval_Callback.Eval.all (Element (D), Env));
         C := Next (D);
      end loop;
   end Add_Defs;


   overriding function Nth (L : Vector_Mal_Type; N : Natural) return Mal_Handle is
   begin
      if N >= L.Length then
         raise Mal_Exception with "Nth (vector): Index out of range";
      else
         return Mal_Vectors.Element (L.Vec, Vec_Index (N));
      end if;
   end Nth;


   -- Get the first item in the list:
   overriding function Car (L : Vector_Mal_Type) return Mal_Handle is
   begin
      return L.Vec.Element (0);
   end Car;

   -- Get the rest of the list (second item onwards)

   overriding function Cdr (L : Vector_Mal_Type) return Mal_Handle is
      Res : Mal_Handle;
      Vec_P : Vector_Ptr;
      C : Mal_Vectors.Cursor;
      I : Vec_Index;
      use Ada.Containers;
   begin
      Res := New_Vector_Mal_Type;
      if L.Vec.Length < 2 then
         return Res;
      end if;
      Vec_P := Deref_Vector (Res);
      Vec_P.Vec := To_Vector (L.Vec.Length - 1);

      -- Set C to second entry.
      C := L.Vec.First;
      Mal_Vectors.Next (C);

      I := 0;
      while Mal_Vectors.Has_Element (C) loop
         Mal_Vectors.Replace_Element (Vec_P.Vec, I, Mal_Vectors.Element (C));
         Mal_Vectors.Next (C);
         I := I + 1;
      end loop;
      return Res;
   end Cdr;

   overriding function Map
     (Func_Ptr : Func_Access;
      L : Vector_Mal_Type)
   return Mal_Handle is
      Res : Mal_Handle;
      use Mal_Vectors;
      C : Cursor;
   begin
      Res := New_Vector_Mal_Type;
      C := First (L.Vec);
      while Has_Element (C) loop
         Deref_Vector (Res).Append (Func_Ptr.all (Element (C)));
         Next (C);
      end loop;
      return Res;
   end Map;


   function Deref_Vector (SP : Mal_Handle) return Vector_Ptr is
   begin
      return Vector_Ptr (Deref (SP));
   end Deref_Vector;


   overriding function To_Str 
     (T : Vector_Mal_Type; Print_Readably : Boolean := True)
   return Mal_String is
     use Ada.Containers;
   begin
      if (T.Vec.Length = 0) then
         return Opening (T.List_Type) &
                Closing (T.List_Type);
      else
         declare
            Res : Ada.Strings.Unbounded.Unbounded_String;
            use Mal_Vectors;
            C : Cursor;
         begin
            C := First (T.Vec);
 
            Res := Ada.Strings.Unbounded."&"
                     (Opening (T.List_Type),
                      Ada.Strings.Unbounded.To_Unbounded_String
                        (To_String (Deref (Element (C)).all, Print_Readably)));
            Next (C);
            while Has_Element (C) loop
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


end Types.Vector;
