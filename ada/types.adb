with Ada.Characters.Latin_1;
with Ada.Strings.Fixed;
with Ada.Strings.Maps.Constants;
with Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with Envs;
with Eval_Callback;
with Smart_Pointers;
with Types.Vector;
with Types.Hash_Map;

package body Types is

   package ACL renames Ada.Characters.Latin_1;

   function Nodes_Equal (A, B : Mal_Handle) return Boolean;


   function "=" (A, B : Mal_Handle) return Mal_Handle is
   begin
      return New_Bool_Mal_Type (A = B);
   end "=";


   function Compare_List_And_Vector (A : List_Mal_Type; B : List_Mal_Type'Class)
   return Boolean is
      First_Node, First_Index : Mal_Handle;
      I : Natural := 0;
   begin
      First_Node := A.The_List;
      loop
         if not Is_Null (First_Node) and I < B.Length then
            First_Index := B.Nth (I);
            if not "=" (Deref_Node (First_Node).Data, First_Index) then
               return False;
            end if;
            First_Node := Deref_Node (First_Node).Next;
            I := I + 1;
         else
            return Is_Null (First_Node) and I = B.Length;
         end if;
      end loop;
   end Compare_List_And_Vector;


   function "=" (A, B : Mal_Handle) return Boolean is
      use Types.Vector;
      use Types.Hash_Map;
   begin

      if (not Is_Null (A) and not Is_Null (B)) and then
         Deref (A).Sym_Type = Deref (B).Sym_Type then

         case Deref (A).Sym_Type is
            when Nil =>
               return True; -- Both nil.
            when Int =>
               return (Deref_Int (A).Get_Int_Val = Deref_Int (B).Get_Int_Val);
            when Floating =>
               return (Deref_Float (A).Get_Float_Val = Deref_Float (B).Get_Float_Val);
            when Bool =>
               return (Deref_Bool (A).Get_Bool = Deref_Bool (B).Get_Bool);
            when List =>
            -- When Types.Vector was added, the choice was:
            -- 1) use interfaces (because you need a class hierachy for the containers
            --    and a corresponding hierarchy for the cursors and Ada is single dispatch
            --    + interfaces.
            -- 2) map out the combinations here and use nth to access vector items.
               case Deref_List (A).Get_List_Type is
                  when List_List =>
                     case Deref_List (B).Get_List_Type is
                        when List_List => 
                           return Nodes_Equal (Deref_List (A).The_List, Deref_List (B).The_List);
                        when Vector_List =>
                           return Compare_List_And_Vector
                                    (Deref_List (A).all, Deref_List_Class (B).all);
                        when Hashed_List => return False; -- Comparing a list and a hash
                     end case;
                  when Vector_List =>
                     case Deref_List (B).Get_List_Type is
                        when List_List =>
                           return Compare_List_And_Vector
                                    (Deref_List (B).all, Deref_List_Class (A).all);
                        when Vector_List =>
                           return Vector."=" (Deref_Vector (A).all, Deref_Vector (B).all);
                        when Hashed_List => return False; -- Comparing a vector and a hash
                     end case;
                  when Hashed_List =>
                     case Deref_List (B).Get_List_Type is
                        when List_List => return False; -- Comparing a list and a hash
                        when Vector_List => return False; -- Comparing a vector and a hash
                        when Hashed_List =>
                           return Hash_Map."=" (Deref_Hash (A).all, Deref_Hash (B).all);
                     end case;
               end case;
            when Str =>
               return (Deref_String (A).Get_String = Deref_String (B).Get_String);
            when Sym =>
               return (Deref_Sym (A).Get_Sym = Deref_Sym (B).Get_Sym);
            when Atom =>
               return (Deref_Atom (A).Get_Atom = Deref_Atom (B).Get_Atom);
            when Func =>
               return (Deref_Func (A).Get_Func_Name = Deref_Func (B).Get_Func_Name);
            when Node =>
               return (Deref_Int(A).Get_Int_Val = Deref_Int(B).Get_Int_Val);
            when Lambda =>
               return (Deref_Int(A).Get_Int_Val = Deref_Int(B).Get_Int_Val);
            when Error =>
               return (Deref_Int(A).Get_Int_Val = Deref_Int(B).Get_Int_Val);
          end case;
      elsif Is_Null (A) and Is_Null (B) then
         return True;
      else  -- either one of the args is null or the sym_types don't match
         return False;
      end if;
   end "=";

   function Get_Meta (T : Mal_Type) return Mal_Handle is
   begin
       if T.Meta = Smart_Pointers.Null_Smart_Pointer then
          return New_Nil_Mal_Type;
       else
          return T.Meta;
       end if;
   end Get_Meta;

   procedure Set_Meta (T : in out Mal_Type'Class; SP : Mal_Handle) is
   begin
      T.Meta := SP;
   end Set_Meta;

   function Copy (M : Mal_Handle) return Mal_Handle is
   begin
      return Smart_Pointers.New_Ptr
        (new Mal_Type'Class'(Deref (M).all));
   end Copy;

   function To_String (T : Mal_Type'Class; Print_Readably : Boolean := True)
   return Mal_String is
   begin
      return To_Str (T, Print_Readably);
   end To_String;

   function Is_Macro_Call (T : Mal_Type'Class; Env : Envs.Env_Handle) return Boolean is
      L : List_Mal_Type;
      First_Elem, Func : Mal_Handle;
   begin

      if T.Sym_Type /= List then
         return False;
      end if;

      L := List_Mal_Type (T);

      if Is_Null (L) then
         return False;
      end if;

      First_Elem := Car (L);

      if Deref (First_Elem).Sym_Type /= Sym then
         return False;
      end if;

      Func := Envs.Get (Env, Deref_Sym (First_Elem).Get_Sym);

      if Deref (Func).Sym_Type /= Lambda then
         return False;
      end if;

      return Deref_Lambda (Func).Get_Is_Macro;

   exception
      when Envs.Not_Found => return False;
   end Is_Macro_Call;


   -- A helper function that just view converts the smart pointer.
   function Deref (S : Mal_Handle) return Mal_Ptr is
   begin
      return Mal_Ptr (Smart_Pointers.Deref (S));
   end Deref;

   -- A helper function to detect null smart pointers.
   function Is_Null (S : Mal_Handle) return Boolean is
      use Smart_Pointers;
   begin
      return Smart_Pointers."="(S, Null_Smart_Pointer);
   end Is_Null;


   -- To_Str on the abstract type...
   function To_Str (T : Mal_Type; Print_Readably : Boolean := True)
   return Mal_String is
   begin
      raise Constraint_Error;  -- Tha'll teach 'ee
      return "";  -- Keeps the compiler happy.
   end To_Str;


   function New_Nil_Mal_Type return Mal_Handle is
   begin
      return Smart_Pointers.New_Ptr
        (new Nil_Mal_Type'(Mal_Type with null record));
   end New_Nil_Mal_Type;

   overriding function Sym_Type (T : Nil_Mal_Type) return Sym_Types is
   begin
      return Nil;
   end Sym_Type;

   overriding function To_Str (T : Nil_Mal_Type; Print_Readably : Boolean := True)
   return Mal_String is
   begin
      return "nil";
   end To_Str;


   function New_Int_Mal_Type (Int : Mal_Integer) return Mal_Handle is
   begin
      return Smart_Pointers.New_Ptr
        (new Int_Mal_Type'(Mal_Type with Int_Val => Int));
   end New_Int_Mal_Type;

   overriding function Sym_Type (T : Int_Mal_Type) return Sym_Types is
   begin
      return Int;
   end Sym_Type;

   function Get_Int_Val (T : Int_Mal_Type) return Mal_Integer is
   begin
      return T.Int_Val;
   end Get_Int_Val;

   overriding function To_Str
     (T : Int_Mal_Type; Print_Readably : Boolean := True)
   return Mal_String is
      Res : Mal_String := Mal_Integer'Image (T.Int_Val);
   begin
      return Ada.Strings.Fixed.Trim (Res, Ada.Strings.Left);
   end To_Str;

   function Deref_Int (SP : Mal_Handle) return Int_Ptr is
   begin
      return Int_Ptr (Deref (SP));
   end Deref_Int;


   function New_Float_Mal_Type (Floating : Mal_Float) return Mal_Handle is
   begin
      return Smart_Pointers.New_Ptr
        (new Float_Mal_Type'(Mal_Type with Float_Val => Floating));
   end New_Float_Mal_Type;

   overriding function Sym_Type (T : Float_Mal_Type) return Sym_Types is
   begin
      return Floating;
   end Sym_Type;

   function Get_Float_Val (T : Float_Mal_Type) return Mal_Float is
   begin
      return T.Float_Val;
   end Get_Float_Val;

   overriding function To_Str 
     (T : Float_Mal_Type; Print_Readably : Boolean := True)
   return Mal_String is
      Res : Mal_String := Mal_Float'Image (T.Float_Val);
   begin
      return Ada.Strings.Fixed.Trim (Res, Ada.Strings.Left);
   end To_Str;

   function Deref_Float (SP : Mal_Handle) return Float_Ptr is
   begin
      return Float_Ptr (Deref (SP));
   end Deref_Float;


   function New_Bool_Mal_Type (Bool : Boolean) return Mal_Handle is
   begin
      return Smart_Pointers.New_Ptr
        (new Bool_Mal_Type'(Mal_Type with Bool_Val => Bool));
   end New_Bool_Mal_Type;

   overriding function Sym_Type (T : Bool_Mal_Type) return Sym_Types is
   begin
      return Bool;
   end Sym_Type;

   function Get_Bool (T : Bool_Mal_Type) return Boolean is
   begin
      return T.Bool_Val;
   end Get_Bool;

   overriding function To_Str 
     (T : Bool_Mal_Type; Print_Readably : Boolean := True)
   return Mal_String is
      Res : Mal_String := Boolean'Image (T.Bool_Val);
   begin
     return Ada.Strings.Fixed.Translate
              (Res, Ada.Strings.Maps.Constants.Lower_Case_Map);
   end To_Str;

   function Deref_Bool (SP : Mal_Handle) return Bool_Ptr is
   begin
      return Bool_Ptr (Deref (SP));
   end Deref_Bool;


   function New_String_Mal_Type (Str : Mal_String) return Mal_Handle is
   begin
      return Smart_Pointers.New_Ptr
        (new String_Mal_Type' (Mal_Type with The_String =>
           Ada.Strings.Unbounded.To_Unbounded_String (Str)));
   end New_String_Mal_Type;

   overriding function Sym_Type (T : String_Mal_Type) return Sym_Types is
   begin
      return Str;
   end Sym_Type;

   function Get_String (T : String_Mal_Type) return Mal_String is
   begin
      return Ada.Strings.Unbounded.To_String (T.The_String);
   end Get_String;

   function Deref_String (SP : Mal_Handle) return String_Ptr is
   begin
      return String_Ptr (Deref (SP));
   end Deref_String;


   overriding function To_Str 
     (T : String_Mal_Type; Print_Readably : Boolean := True)
   return Mal_String is
      use Ada.Strings.Unbounded;
      I : Positive := 1;
      Str_Len : Natural;
      Res : Unbounded_String;
      Ch : Character;
   begin
      if Print_Readably then
         Append (Res, '"');
         Str_Len := Length (T.The_String);
         while I <= Str_Len loop
            Ch := Element (T.The_String, I);
            if Ch = '"' then
               Append (Res, "\""");
            elsif Ch = '\' then
               Append (Res, "\\");
            elsif Ch = Ada.Characters.Latin_1.LF then
               Append (Res, "\n");
            else
               Append (Res, Ch);
            end if;
            I := I + 1;
         end loop;
         Append (Res, '"');
         return To_String (Res);
      else
         return To_String (T.The_String);
      end if;
   end To_Str;


   function New_Symbol_Mal_Type (Str : Mal_String) return Mal_Handle is
   begin
      return Smart_Pointers.New_Ptr
        (new Symbol_Mal_Type'(Mal_Type with The_Symbol =>
           Ada.Strings.Unbounded.To_Unbounded_String (Str)));
   end New_Symbol_Mal_Type;

   overriding function Sym_Type (T : Symbol_Mal_Type) return Sym_Types is
   begin
      return Sym;
   end Sym_Type;

   function Get_Sym (T : Symbol_Mal_Type) return Mal_String is
   begin
      return Ada.Strings.Unbounded.To_String (T.The_Symbol);
   end Get_Sym;

   function Deref_Sym (S : Mal_Handle) return Sym_Ptr is
   begin
      return Sym_Ptr (Deref (S));
   end Deref_Sym;

   overriding function To_Str 
     (T : Symbol_Mal_Type; Print_Readably : Boolean := True)
   return Mal_String is
   begin
      return Ada.Strings.Unbounded.To_String (T.The_Symbol);
   end To_Str;


   function New_Atom_Mal_Type (MH : Mal_Handle) return Mal_Handle is
   begin
      return Smart_Pointers.New_Ptr
        (new Atom_Mal_Type'(Mal_Type with The_Atom => MH));
   end New_Atom_Mal_Type;

   overriding function Sym_Type (T : Atom_Mal_Type) return Sym_Types is
   begin
      return Atom;
   end Sym_Type;

   function Get_Atom (T : Atom_Mal_Type) return Mal_Handle is
   begin
      return T.The_Atom;
   end Get_Atom;

   procedure Set_Atom (T : in out Atom_Mal_Type; New_Val : Mal_Handle) is
   begin
      T.The_Atom := New_Val;
   end Set_Atom;

   function Deref_Atom (S : Mal_Handle) return Atom_Ptr is
   begin
      return Atom_Ptr (Deref (S));
   end Deref_Atom;

   overriding function To_Str 
     (T : Atom_Mal_Type; Print_Readably : Boolean := True)
   return Mal_String is
   begin
      return "(atom " & To_String (Deref (T.The_Atom).all) & ')';
   end To_Str;


   function New_Func_Mal_Type (Str : Mal_String; F : Builtin_Func)
   return Mal_Handle is
   begin
      return Smart_Pointers.New_Ptr
        (new Func_Mal_Type'(Mal_Type with
          Func_Name => Ada.Strings.Unbounded.To_Unbounded_String (Str),
          Func_P => F));
   end New_Func_Mal_Type;

   overriding function Sym_Type (T : Func_Mal_Type) return Sym_Types is
   begin
      return Func;
   end Sym_Type;

   function Get_Func_Name (T : Func_Mal_Type) return Mal_String is
   begin
      return Ada.Strings.Unbounded.To_String (T.Func_Name);
   end Get_Func_Name;

   function Call_Func
     (FMT : Func_Mal_Type; Rest_List : Mal_Handle)
   return Mal_Handle is
   begin
      return FMT.Func_P (Rest_List);
   end Call_Func;

   function Deref_Func (S : Mal_Handle) return Func_Ptr is
   begin
      return Func_Ptr (Deref (S));
   end Deref_Func;

   overriding function To_Str 
     (T : Func_Mal_Type; Print_Readably : Boolean := True)
   return Mal_String is
   begin
      return Ada.Strings.Unbounded.To_String (T.Func_Name);
   end To_Str;


   function New_Error_Mal_Type (Str : Mal_String) return Mal_Handle is
   begin
      return Smart_Pointers.New_Ptr
        (new Error_Mal_Type'(Mal_Type with Error_Msg =>
           Ada.Strings.Unbounded.To_Unbounded_String (Str)));
   end New_Error_Mal_Type;

   overriding function Sym_Type (T : Error_Mal_Type) return Sym_Types is
   begin
      return Error;
   end Sym_Type;

   overriding function To_Str 
     (T : Error_Mal_Type; Print_Readably : Boolean := True)
   return Mal_String is
   begin
      return Ada.Strings.Unbounded.To_String (T.Error_Msg);
   end To_Str;


   function Nodes_Equal (A, B : Mal_Handle) return Boolean is
   begin
      if (not Is_Null (A) and not Is_Null (B)) and then
         Deref (A).Sym_Type = Deref (B).Sym_Type then
         if Deref (A).Sym_Type = Node then
            return
              Nodes_Equal (Deref_Node (A).Data, Deref_Node (B).Data) and then
              Nodes_Equal (Deref_Node (A).Next, Deref_Node (B).Next);
         else
            return A = B; 
         end if;
      elsif Is_Null (A) and Is_Null (B) then
         return True;
      else  -- either one of the args is null or the sym_types don't match
         return False;
      end if;
   end Nodes_Equal;


   function New_Node_Mal_Type
     (Data : Mal_Handle;
      Next : Mal_Handle := Smart_Pointers.Null_Smart_Pointer)
   return Mal_Handle is
   begin
      return Smart_Pointers.New_Ptr
        (new Node_Mal_Type'
          (Mal_Type with Data => Data, Next => Next));
   end New_Node_Mal_Type;


   overriding function Sym_Type (T : Node_Mal_Type) return Sym_Types is
   begin
      return Node;
   end Sym_Type;


   -- Get the first item in the list:
   function Car (L : List_Mal_Type) return Mal_Handle is
   begin
      if Is_Null (L.The_List) then
         return Smart_Pointers.Null_Smart_Pointer;
      else
         return Deref_Node (L.The_List).Data;
      end if;
   end Car;
   
   
   -- Get the rest of the list (second item onwards)
   function Cdr (L : List_Mal_Type) return Mal_Handle is
      Res : Mal_Handle;
      LP : List_Ptr;
   begin

      Res := New_List_Mal_Type (L.List_Type);

      if Is_Null (L.The_List) or else
         Is_Null (Deref_Node (L.The_List).Next) then
         return Res;
      else
         LP := Deref_List (Res);
         LP.The_List := Deref_Node (L.The_List).Next;
         LP.Last_Elem := L.Last_Elem;
         return Res;
      end if;
   end Cdr;


   function Length (L : List_Mal_Type) return Natural is
      Res : Natural;
      NP : Node_Ptr;
   begin
      Res := 0;
      NP := Deref_Node (L.The_List);
      while NP /= null loop
         Res := Res + 1;
         NP := Deref_Node (NP.Next);
      end loop;
      return Res;
   end Length;


   function Is_Null (L : List_Mal_Type) return Boolean is
      use Smart_Pointers;
   begin
      return Smart_Pointers."="(L.The_List, Null_Smart_Pointer);
   end Is_Null;


   function Null_List (L : List_Types) return List_Mal_Type is
   begin
      return (Mal_Type with List_Type => L,
              The_List => Smart_Pointers.Null_Smart_Pointer,
              Last_Elem => Smart_Pointers.Null_Smart_Pointer);
   end Null_List;


   function Map
     (Func_Ptr : Func_Access;
      L : List_Mal_Type)
   return Mal_Handle is

      Res, Old_List, First_New_Node, New_List : Mal_Handle;
      LP : List_Ptr;

   begin

      Res := New_List_Mal_Type (List_Type => L.Get_List_Type);

      Old_List := L.The_List;

      if Is_Null (Old_List) then
         return Res;
      end if;

      First_New_Node := New_Node_Mal_Type (Func_Ptr.all (Deref_Node (Old_List).Data));

      New_List := First_New_Node;

      Old_List := Deref_Node (Old_List).Next;

      while not Is_Null (Old_List) loop

         Deref_Node (New_List).Next :=
           New_Node_Mal_Type (Func_Ptr.all (Deref_Node (Old_List).Data));

         New_List := Deref_Node (New_List).Next;

         Old_List := Deref_Node (Old_List).Next;

      end loop;

      LP := Deref_List (Res);
      LP.The_List := First_New_Node;
      LP.Last_Elem := New_List;

      return Res;

   end Map;


   function Reduce
     (Func_Ptr : Binary_Func_Access;
      L : List_Mal_Type)
   return Mal_Handle is

      C_Node : Node_Ptr;
      Res : Mal_Handle;
      use Smart_Pointers;

   begin

      C_Node := Deref_Node (L.The_List);

      if C_Node = null then
         return Smart_Pointers.Null_Smart_Pointer;
      end if;

      Res := C_Node.Data;
      while not Is_Null (C_Node.Next) loop
         C_Node := Deref_Node (C_Node.Next);
         Res := Func_Ptr (Res, C_Node.Data);
      end loop;

      return Res;

   end Reduce;


   overriding function To_Str 
     (T : Node_Mal_Type; Print_Readably : Boolean := True)
   return Mal_String is
   begin
      if Is_Null (T.Data) then
         -- Left is null and by implication so is right.
         return "";
      elsif Is_Null (T.Next) then
        -- Left is not null but right is.
        return To_Str (Deref (T.Data).all, Print_Readably);
      else
        -- Left and right are both not null.
        return To_Str (Deref (T.Data).all, Print_Readably) &
               " " &
               To_Str (Deref (T.Next).all, Print_Readably);
      end if;
   end To_Str;


   function Cat_Str (T : Node_Mal_Type; Print_Readably : Boolean := True)
   return Mal_String is
   begin
      if Is_Null (T.Data) then
         -- Left is null and by implication so is right.
         return "";
      elsif Is_Null (T.Next) then
        -- Left is not null but right is.
        return To_Str (Deref (T.Data).all, Print_Readably);

      -- Left and right are both not null.
      else
        return To_Str (Deref (T.Data).all, Print_Readably) &
               Cat_Str (Deref_Node (T.Next).all, Print_Readably);
      end if;
   end Cat_Str;


   function Deref_Node (SP : Mal_Handle) return Node_Ptr is
   begin
      return Node_Ptr (Deref (SP));
   end Deref_Node;


   function "=" (A, B : List_Mal_Type) return Boolean is
   begin
      return Nodes_Equal (A.The_List, B.The_List);
   end "=";

   function New_List_Mal_Type
     (The_List : List_Mal_Type)
   return Mal_Handle is
   begin
     return Smart_Pointers.New_Ptr
        (new List_Mal_Type'(Mal_Type with
          List_Type => The_List.List_Type,
          The_List => The_List.The_List,
          Last_Elem => The_List.Last_Elem));
   end New_List_Mal_Type;


   function New_List_Mal_Type
     (List_Type : List_Types;
      The_First_Node : Mal_Handle := Smart_Pointers.Null_Smart_Pointer)
   return Mal_Handle is
   begin
      return Smart_Pointers.New_Ptr
        (new List_Mal_Type'
          (Mal_Type with
            List_Type => List_Type,
            The_List => The_First_Node,
            Last_Elem => The_First_Node));
   end New_List_Mal_Type;


   function Make_New_List (Handle_List : Handle_Lists) return Mal_Handle is

      List_SP : Mal_Handle;
      List_P : List_Ptr;

   begin
      List_SP := New_List_Mal_Type (List_Type => List_List);
      List_P := Deref_List (List_SP);
      for I in Handle_List'Range loop
         Append (List_P.all, Handle_List (I));
      end loop;
      return List_SP;
   end Make_New_List;


   overriding function Sym_Type (T : List_Mal_Type) return Sym_Types is
   begin
      return List;
   end Sym_Type;


   function Get_List_Type (L : List_Mal_Type) return List_Types is
   begin
      return L.List_Type;
   end Get_List_Type;


   function Prepend (Op : Mal_Handle; To_List : List_Mal_Type)
   return Mal_Handle is
   begin
      return New_List_Mal_Type
               (List_List,
                New_Node_Mal_Type (Op, To_List.The_List));
   end Prepend;


   procedure Append (To_List : in out List_Mal_Type; Op : Mal_Handle) is
   begin
      if Is_Null (Op) then
         return;  -- Say what
      end if;

      -- If the list is null just insert the new element
      -- else use the last_elem pointer to insert it and then update it.
      if Is_Null (To_List.The_List) then
         To_List.The_List := New_Node_Mal_Type (Op);
         To_List.Last_Elem := To_List.The_List;
      else
         Deref_Node (To_List.Last_Elem).Next := New_Node_Mal_Type (Op);
         To_List.Last_Elem := Deref_Node (To_List.Last_Elem).Next;
      end if;
   end Append;


   -- Duplicate copies the list (logically).  This is to allow concatenation,
   -- The result is always a List_List.
   function Duplicate (The_List : List_Mal_Type) return Mal_Handle is
      Res, Old_List, First_New_Node, New_List : Mal_Handle;
      LP : List_Ptr;
   begin

      Res := New_List_Mal_Type (List_List);

      Old_List := The_List.The_List;

      if Is_Null (Old_List) then
         return Res;
      end if;

      First_New_Node := New_Node_Mal_Type (Deref_Node (Old_List).Data);
      New_List := First_New_Node;
      Old_List := Deref_Node (Old_List).Next;

      while not Is_Null (Old_List) loop

         Deref_Node (New_List).Next := New_Node_Mal_Type (Deref_Node (Old_List).Data);
         New_List := Deref_Node (New_List).Next;
         Old_List := Deref_Node (Old_List).Next;

      end loop;

      LP := Deref_List (Res);
      LP.The_List := First_New_Node;
      LP.Last_Elem := New_List;

      return Res;

   end Duplicate;


   function Nth (L : List_Mal_Type; N : Natural) return Mal_Handle is

      C : Natural;
      Next : Mal_Handle;

   begin

      C := 0;

      Next := L.The_List;

      while not Is_Null (Next) loop

         if C >= N then
            return Deref_Node (Next).Data;
         end if;

         C := C + 1;

         Next := Deref_Node (Next).Next;

      end loop;

      raise Mal_Exception with "Nth (list): Index out of range";

   end Nth;


   function Concat (Rest_Handle : List_Mal_Type)
   return Types.Mal_Handle is
      Rest_List : Types.List_Mal_Type;
      List : Types.List_Class_Ptr;
      Res_List_Handle, Dup_List : Mal_Handle;
      Last_Node_P : Mal_Handle := Smart_Pointers.Null_Smart_Pointer;
   begin
      Rest_List := Rest_Handle;

      -- Set the result to the null list.
      Res_List_Handle := New_List_Mal_Type (List_List);

      while not Is_Null (Rest_List) loop

         -- Find the next list in the list...
         List := Deref_List_Class (Car (Rest_List));

         -- Duplicate nodes to its contents. 
         Dup_List := Duplicate (List.all);

         -- If we haven't inserted a list yet, then take the duplicated list whole.
         if Is_Null (Last_Node_P) then
            Res_List_Handle := Dup_List;
         else
            -- Note that the first inserted list may have been the null list
            -- and so may the newly duplicated one...
            Deref_Node (Last_Node_P).Next := Deref_List (Dup_List).The_List;
            if Is_Null (Deref_List (Res_List_Handle).The_List) then
               Deref_List (Res_list_Handle).The_List :=
                 Deref_List (Dup_List).The_List;
            end if;
            if not Is_Null (Deref_List (Dup_List).Last_Elem) then
               Deref_List (Res_List_Handle).Last_Elem :=
                 Deref_List (Dup_List).Last_Elem;
            end if;
         end if;

         Last_Node_P := Deref_List (Dup_List).Last_Elem;

         Rest_List := Deref_List (Cdr (Rest_List)).all;

      end loop;

      return Res_List_Handle;

   end Concat;


   procedure Add_Defs (Defs : List_Mal_Type; Env : Envs.Env_Handle) is
      D, L : List_Mal_Type;
   begin
      D := Defs;
      while not Is_Null (D) loop
         L := Deref_List (Cdr (D)).all;
         Envs.Set
           (Env,
            Deref_Sym (Car (D)).Get_Sym,
            Eval_Callback.Eval.all (Car (L), Env));
         D := Deref_List (Cdr(L)).all;
      end loop;
   end Add_Defs;


   function Deref_List (SP : Mal_Handle) return List_Ptr is
   begin
      return List_Ptr (Deref (SP));
   end Deref_List;


   function Deref_List_Class (SP : Mal_Handle) return List_Class_Ptr is
   begin
      return List_Class_Ptr (Deref (SP));
   end Deref_List_Class;


   overriding function To_Str 
     (T : List_Mal_Type; Print_Readably : Boolean := True)
   return Mal_String is
   begin
      if Is_Null (T.The_List) then
         return Opening (T.List_Type) &
                Closing (T.List_Type);
      else
         return Opening (T.List_Type) &
                To_String (Deref (T.The_List).all, Print_Readably) &
                Closing (T.List_Type);
      end if;
   end To_Str;


   function Pr_Str (T : List_Mal_Type; Print_Readably : Boolean := True)
   return Mal_String is
   begin
      if Is_Null (T.The_List) then
         return "";
      else
         return To_String (Deref_Node (T.The_List).all, Print_Readably);
      end if;
   end Pr_Str;


   function Cat_Str (T : List_Mal_Type; Print_Readably : Boolean := True)
   return Mal_String is
   begin
      if Is_Null (T.The_List) then
         return "";
      else
         return Cat_Str (Deref_Node (T.The_List).all, Print_Readably);
      end if;
   end Cat_Str;


   function Opening (LT : List_Types) return Character is
      Res : Character;
   begin
      case LT is
         when List_List =>
            Res := '(';
         when Vector_List =>
            Res := '[';
         when Hashed_List =>
            Res := '{';
      end case;
      return Res;
   end Opening;


   function Closing (LT : List_Types) return Character is
      Res : Character;
   begin
      case LT is
         when List_List =>
            Res := ')';
         when Vector_List =>
            Res := ']';
         when Hashed_List =>
            Res := '}';
      end case;
      return Res;
   end Closing;


   function New_Lambda_Mal_Type
     (Params : Mal_Handle; Expr : Mal_Handle; Env : Envs.Env_Handle)
   return Mal_Handle is
   begin
      return Smart_Pointers.New_Ptr
        (new Lambda_Mal_Type'
          (Mal_Type with
             Params => Params,
             Expr => Expr,
             Env => Env,
             Is_Macro => False));
   end New_Lambda_Mal_Type;

   overriding function Sym_Type (T : Lambda_Mal_Type) return Sym_Types is
   begin
      return Lambda;
   end Sym_Type;

   function Get_Env (L : Lambda_Mal_Type) return Envs.Env_Handle is
   begin
      return L.Env;
   end Get_Env;

   procedure Set_Env (L : in out Lambda_Mal_Type; Env : Envs.Env_Handle) is
   begin
      L.Env := Env;
   end Set_Env;

   function Get_Params (L : Lambda_Mal_Type) return Mal_Handle is
   begin
      if Deref (L.Params).Sym_Type = List and then
         Deref_List (L.Params).Get_List_Type = Vector_List then
         -- Its a vector and we need a list...
         return Deref_List_Class (L.Params).Duplicate;
      else
         return L.Params;
      end if;
   end Get_Params;

   function Get_Expr (L : Lambda_Mal_Type) return Mal_Handle is
   begin
      return L.Expr;
   end Get_Expr;

   function Get_Is_Macro (L : Lambda_Mal_Type) return Boolean is
   begin
      return L.Is_Macro;
   end Get_Is_Macro;

   procedure Set_Is_Macro (L : in out Lambda_Mal_Type; B : Boolean) is
   begin
      L.Is_Macro := B;
   end Set_Is_Macro;


   function Apply
     (L : Lambda_Mal_Type;
      Param_List : Mal_Handle)
   return Mal_Handle is

      E : Envs.Env_Handle;
      Param_Names : List_Mal_Type;
      Res : Mal_Handle;

   begin

      E := Envs.New_Env (L.Env);

      Param_Names := Deref_List (L.Get_Params).all;

      if Envs.Bind (E, Param_Names, Deref_List (Param_List).all) then

         Res := Eval_Callback.Eval.all (L.Get_Expr, E); 

      else

         raise Mal_Exception with "Bind failed in Apply";

      end if;

      return Res;

   end Apply;


   function Get_Macro (T : Mal_Handle; Env : Envs.Env_Handle) return Lambda_Ptr is
      L : List_Mal_Type;
      First_Elem, Func : Mal_Handle;
   begin

      if Deref (T).Sym_Type /= List then
         return null;
      end if;

      L := Deref_List (T).all;

      if Is_Null (L) then
         return null;
      end if;

      First_Elem := Car (L);

      if Deref (First_Elem).Sym_Type /= Sym then
         return null;
      end if;

      Func := Envs.Get (Env, Deref_Sym (First_Elem).Get_Sym);

      if Deref (Func).Sym_Type /= Lambda then
         return null;
      end if;

      return Deref_Lambda (Func);

   exception
      when Envs.Not_Found => return null;
   end Get_Macro;


   overriding function To_Str 
     (T : Lambda_Mal_Type; Print_Readably : Boolean := True)
   return Mal_String is
   begin
--      return "(lambda " & Ada.Strings.Unbounded.To_String (T.Rep) & ")";
      return "#<function>";
   end To_Str;

   function Deref_Lambda (SP : Mal_Handle) return Lambda_Ptr is
   begin
      return Lambda_Ptr (Deref (SP));
   end Deref_Lambda;


   function Arith_Op (A, B : Mal_Handle) return Mal_Handle is
      use Types;
      A_Sym_Type : Sym_Types;
      B_Sym_Type : Sym_Types;
   begin

      if Is_Null (A) then
        if Is_Null (B) then
           -- both null, gotta be zero.
           return New_Int_Mal_Type (0);
        else  -- A is null but B is not.
           return Arith_Op (New_Int_Mal_Type (0), B);
        end if;
      elsif Is_Null (B) then
        -- A is not null but B is.
         return Arith_Op (A, New_Int_Mal_Type (0));
      end if;
  
      -- else both A and B and not null.:wq
      A_Sym_Type := Deref (A).Sym_Type;
      B_Sym_Type := Deref (B).Sym_Type;
      if A_Sym_Type = Int and B_Sym_Type = Int then
         return New_Int_Mal_Type
           (Int_Op (Deref_Int (A).Get_Int_Val, Deref_Int (B).Get_Int_Val));
      elsif A_Sym_Type = Int and B_Sym_Type = Floating then
         return New_Float_Mal_Type
           (Float_Op (Mal_Float (Deref_Int (A).Get_Int_Val),
            Deref_Float (B).Get_Float_Val));
      elsif A_Sym_Type = Floating and B_Sym_Type = Int then
         return New_Float_Mal_Type
           (Float_Op (Deref_Float (A).Get_Float_Val,
            Mal_Float (Deref_Float (B).Get_Float_Val)));
      elsif A_Sym_Type = Floating and B_Sym_Type = Floating then
         return New_Float_Mal_Type
           (Float_Op (Deref_Float (A).Get_Float_Val,
            Deref_Float (B).Get_Float_Val));
      else
         if A_Sym_Type = Error then
            return A;
         elsif B_Sym_Type = Error then
            return B;
         else
            return New_Error_Mal_Type ("Invalid operands");
         end if;
      end if;
   end Arith_Op;


   function Rel_Op (A, B : Mal_Handle) return Mal_Handle is
      use Types;
      A_Sym_Type : Sym_Types := Deref (A).Sym_Type;
      B_Sym_Type : Sym_Types := Deref (B).Sym_Type;
   begin
      if A_Sym_Type = Int and B_Sym_Type = Int then
         return New_Bool_Mal_Type
           (Int_Rel_Op (Deref_Int (A).Get_Int_Val, Deref_Int (B).Get_Int_Val));
      elsif A_Sym_Type = Int and B_Sym_Type = Floating then
         return New_Bool_Mal_Type
           (Float_Rel_Op (Mal_Float (Deref_Int (A).Get_Int_Val),
            Deref_Float (B).Get_Float_Val));
      elsif A_Sym_Type = Floating and B_Sym_Type = Int then
         return New_Bool_Mal_Type
           (Float_Rel_Op (Deref_Float (A).Get_Float_Val,
            Mal_Float (Deref_Float (B).Get_Float_Val)));
      else
         return New_Bool_Mal_Type
           (Float_Rel_Op (Deref_Float (A).Get_Float_Val,
            Deref_Float (B).Get_Float_Val));
      end if;
   end Rel_Op;


end Types;
