with Ada.Characters.Latin_1;
with Ada.Strings.Fixed;
with Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with Smart_Pointers;

package body Types is

   package ACL renames Ada.Characters.Latin_1;


   function Get_Meta (T : Mal_Type) return Smart_Pointer is
   begin
       return T.Meta;
   end Get_Meta;

   procedure Set_Meta (T : in out Mal_Type'Class; SP : Smart_Pointer) is
   begin
      T.Meta := SP;
   end Set_Meta;

   function To_String (T : Mal_Type'Class) return Mal_String is
   begin
      if not Is_Null (T.Meta) then
         return "(with-meta " &
                To_Str (T) & " " &
                To_Str (Deref (T.Meta).all) & ")";
      else
         return To_Str (T);
      end if;
   end To_String;


   -- A helper function that just view converts the smart pointer.
   function Deref (S : Smart_Pointer) return Mal_Ptr is
   begin
      return Mal_Ptr (Smart_Pointers.Deref (S));
   end Deref;

   -- A helper function to detect null smart pointers.
   function Is_Null (S : Smart_Pointer) return Boolean is
      use Smart_Pointers;
   begin
      return S = Null_Smart_Pointer;
   end Is_Null;


   -- To_Str on the abstract type...
   function To_Str (T : Mal_Type) return Mal_String is
   begin
      raise Constraint_Error;  -- Tha'll teach 'ee
      return "";  -- Keeps the compiler happy.
   end To_Str;


   function New_Int_Mal_Type (Int : Mal_Integer) return Smart_Pointer is
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

   overriding function To_Str (T : Int_Mal_Type) return Mal_String is
      Res : Mal_String := Mal_Integer'Image (T.Int_Val);
   begin
      return Ada.Strings.Fixed.Trim (Res, Ada.Strings.Left);
   end To_Str;

   function Deref_Int (SP : Smart_Pointer) return Int_Ptr is
   begin
      return Int_Ptr (Deref (SP));
   end Deref_Int;


   function New_Float_Mal_Type (Floating : Mal_Float) return Smart_Pointer is
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

   overriding function To_Str (T : Float_Mal_Type) return Mal_String is
      Res : Mal_String := Mal_Float'Image (T.Float_Val);
   begin
      return Ada.Strings.Fixed.Trim (Res, Ada.Strings.Left);
   end To_Str;

   function Deref_Float (SP : Smart_Pointer) return Float_Ptr is
   begin
      return Float_Ptr (Deref (SP));
   end Deref_Float;


   function New_Sym_Mal_Type (Sym : Character) return Smart_Pointer is
   begin
      return Smart_Pointers.New_Ptr
        (new Sym_Mal_Type'(Mal_Type with Symbol => Sym));
   end New_Sym_Mal_Type;

   overriding function Sym_Type (T : Sym_Mal_Type) return Sym_Types is
   begin
      return Sym;
   end Sym_Type;

   function Symbol (T : Sym_Mal_Type) return Character is
   begin
      return T.Symbol;
   end Symbol;

   overriding function To_Str (T : Sym_Mal_Type) return Mal_String is
   begin
      return "" & T.Symbol;
   end To_Str;

   function Deref_Sym (S : Smart_Pointer) return Sym_Ptr is
   begin
      return Sym_Ptr (Deref (S));
   end Deref_Sym;


   function New_String_Mal_Type (Str : Mal_String) return Smart_Pointer is
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

   overriding function To_Str (T : String_Mal_Type) return Mal_String is
   begin
      return Ada.Strings.Unbounded.To_String (T.The_String);
   end To_Str;


   function New_Atom_Mal_Type (Str : Mal_String) return Smart_Pointer is
   begin
      return Smart_Pointers.New_Ptr
        (new Atom_Mal_Type'(Mal_Type with The_Atom =>
           Ada.Strings.Unbounded.To_Unbounded_String (Str)));
   end New_Atom_Mal_Type;

   overriding function Sym_Type (T : Atom_Mal_Type) return Sym_Types is
   begin
      return Atom;
   end Sym_Type;

   function Get_Atom (T : Atom_Mal_Type) return Mal_String is
   begin
      return Ada.Strings.Unbounded.To_String (T.The_Atom);
   end Get_Atom;

   overriding function To_Str (T : Atom_Mal_Type) return Mal_String is
   begin
      return Ada.Strings.Unbounded.To_String (T.The_Atom);
   end To_Str;


   function New_Error_Mal_Type (Str : Mal_String) return Smart_Pointer is
   begin
      return Smart_Pointers.New_Ptr
        (new Error_Mal_Type'(Mal_Type with Error_Msg =>
           Ada.Strings.Unbounded.To_Unbounded_String (Str)));
   end New_Error_Mal_Type;

   overriding function Sym_Type (T : Error_Mal_Type) return Sym_Types is
   begin
      return Error;
   end Sym_Type;

   overriding function To_Str (T : Error_Mal_Type) return Mal_String is
   begin
      return Ada.Strings.Unbounded.To_String (T.Error_Msg);
   end To_Str;


   function New_Unitary_Mal_Type (Func : Unitary_Functions; Op : Smart_Pointer)
   return Smart_Pointer is
   begin
      return Smart_Pointers.New_Ptr
        (new Unitary_Mal_Type'
           (Mal_Type with The_Function => Func, The_Operand => Op));
   end New_Unitary_Mal_Type;

   overriding function Sym_Type (T : Unitary_Mal_Type) return Sym_Types is
   begin
      return Unitary;
   end Sym_Type;

   function Get_Func (T : Unitary_Mal_Type) return Unitary_Functions is
   begin
      return T.The_Function;
   end Get_Func;

   function Get_Op (T : Unitary_Mal_Type) return Smart_Pointer is
   begin
     return T.The_Operand;
   end Get_Op;

   function Map_Nodes
     (Func_Ptr : Func_Access;
      L : Node_Mal_Type)
   return Types.Smart_Pointer is
   begin
      if not Is_Null (L.Right) then
         if Deref (L.Right).Sym_Type = Node then
            return New_Node_Mal_Type
                     (Left => Func_Ptr.all (L.Left),
                      Right => Map_Nodes (Func_Ptr, Deref_Node (L.Right).all));
         else
            -- Left and right are both filled.
            return New_Node_Mal_Type
                     (Left => Func_Ptr.all (L.Left),
                      Right => Func_Ptr.all (L.Right));
         end if;
      else  -- Right is null.
         return New_Node_Mal_Type
                  (Left => Func_Ptr.all (L.Left),
                   Right => Smart_Pointers.Null_Smart_Pointer);
      end if;
   end Map_Nodes;


   function Reduce_Nodes
     (Func_Ptr : Binary_Func_Access;
      L : Node_Mal_Type)
   return Types.Smart_Pointer is
      C_Node : Node_Mal_Type := L;
      Res : Types.Smart_Pointer;
   begin
      if Is_Null (C_Node.Left) then
         return Smart_Pointers.Null_Smart_Pointer;
      end if;
      Res := C_Node.Left;
      while not Is_Null (C_Node.Right) and then
            Deref (C_Node.Right).Sym_Type = Node loop

         C_Node := Deref_Node (C_Node.Right).all;
         Res := Func_Ptr (Res, C_Node.Left);
      end loop;
      if not Is_Null (C_Node.Right) then
         Res := Func_Ptr (Res, C_Node.Right);
      end if;
      return Res;
   end Reduce_Nodes;


   overriding function To_Str (T : Unitary_Mal_Type) return Mal_String is
   begin
      case T.The_Function is
         when Quote =>
            return "(quote " & To_String (Deref (T.The_Operand).all) & ")";
         when Unquote =>
            return "(unquote " & To_String (Deref (T.The_Operand).all) & ")";
         when Quasiquote =>
            return "(quasiquote " & To_String (Deref (T.The_Operand).all) & ")";
         when Splice_Unquote =>
            return
              "(splice-unquote " & To_String (Deref (T.The_Operand).all) & ")";
         when Deref =>
            return
              "(deref " & To_String (Deref (T.The_Operand).all) & ")";
      end case;
   end To_Str;


   function New_Node_Mal_Type (Left, Right : Smart_Pointer :=
                        Smart_Pointers.Null_Smart_pointer)
   return Smart_Pointer is
   begin
      return Smart_Pointers.New_Ptr
        (new Node_Mal_Type' (Mal_Type with Left => Left, Right => Right));
   end New_Node_Mal_Type;

   overriding function Sym_Type (T : Node_Mal_Type) return Sym_Types is
   begin
      return Node;
   end Sym_Type;

   procedure Append (To_List : in out Node_Mal_Type; Op : Smart_Pointer) is
   begin
      if Is_Null (To_List.Left) then
         To_List.Left := Op;
      elsif Is_Null (To_List.Right) then
         To_List.Right := Op;
      elsif Sym_Type (Deref (To_List.Right).all) = Node then
         declare
            Node_P : Node_Ptr;
         begin
            Node_P := Deref_Node (To_List.Right);
            Append (Node_P.all, Op);
         end;
      else
         -- Right is not null and not a node i.e. a full node.
         To_List.Right := Types.New_Node_Mal_Type
            (Left => To_List.Right,
             Right => Op);
      end if;
   end Append;

   -- Get the first item in the list:
   function Car (L : List_Mal_Type) return Smart_Pointer is
   begin
      if Is_Null (L.The_List) then
         return Smart_Pointers.Null_Smart_Pointer;
      else
         return Deref_Node (L.The_List).Left;
      end if;
   end Car;
   
   
   -- Get the rest of the list (second item onwards)
   function Cdr (L : List_Mal_Type) return List_Mal_Type is
   begin
      if Is_Null (L.The_List) then
         return L;
      end if;
      declare
         Node_P : Node_Ptr;
      begin 
         Node_P := Deref_Node (L.The_List);
         -- Clojure lists are constants?
         -- If not, need to copy P.Right to a new list...
         -- Or maybe we copy on write?
         return Deref_List (New_List_Mal_Type (L.List_Type, Node_P.Right)).all;
      end;
   end Cdr;

   function Null_List (L : List_Types) return List_Mal_Type is
   begin
      return (Mal_Type with List_Type => L,
              The_List => Smart_Pointers.Null_Smart_Pointer);
   end Null_List;


   function Map
     (Func_Ptr : Func_Access;
      L : List_Mal_Type)
   return Types.Smart_Pointer is
   begin
      if Is_Null (L.The_List) then
         return New_List_Mal_Type (L.Get_List_Type);
      else
         return New_List_Mal_Type
           (L.Get_List_Type,
            Map_Nodes (Func_Ptr, Deref_Node (L.The_List).all));
      end if;
   end Map;

   function Reduce
     (Func_Ptr : Binary_Func_Access;
      L : List_Mal_Type)
   return Types.Smart_Pointer is
   begin
      if Is_Null (L.The_List) then
         return New_List_Mal_Type (L.Get_List_Type);
      else
         return Reduce_Nodes (Func_Ptr, Deref_Node (L.The_List).all);
      end if;
   end Reduce;

   overriding function To_Str (T : Node_Mal_Type) return Mal_String is
   begin
      if Is_Null (T.Left) then
         -- Left is null and by implication so is right.
         return "";
      elsif Is_Null (T.Right) then
        -- Left is not null but right is.
        return To_Str (Deref (T.Left).all);
      else
        -- Left and right are both not null.
        return To_Str (Deref (T.Left).all) &
               " " &
               To_Str (Deref (T.Right).all);
      end if;
   end To_Str;

   function Deref_Node (SP : Smart_Pointer) return Node_Ptr is
   begin
      return Node_Ptr (Deref (SP));
   end Deref_Node;



   function New_List_Mal_Type
     (List_Type : List_Types;
      The_First_Node : Smart_Pointer := Smart_Pointers.Null_Smart_Pointer)
   return Smart_Pointer is
   begin
      return Smart_Pointers.New_Ptr
        (new List_Mal_Type'(Mal_Type with
          List_Type => List_Type,
          The_List => The_First_Node));
   end New_List_Mal_Type;

   overriding function Sym_Type (T : List_Mal_Type) return Sym_Types is
   begin
      return List;
   end Sym_Type;

   function Get_List_Type (L : List_Mal_Type) return List_Types is
   begin
      return L.List_Type;
   end Get_List_Type;

   procedure Append (To_List : in out List_Mal_Type; Op : Smart_Pointer) is
      Node_P : Node_Ptr;
   begin
      if Is_Null (Op) then
         return;  -- Say what
      end if;
      if Is_Null (To_List.The_List) then
         To_List.The_List := New_Node_Mal_Type;
      end if;
      Node_P := Deref_Node (To_List.The_List);
      Append (Node_P.all, Op);
   end Append;

   function Deref_List (SP : Smart_Pointer) return List_Ptr is
   begin
      return List_Ptr (Deref (SP));
   end Deref_List;


   overriding function To_Str (T : List_Mal_Type) return Mal_String is
   begin
      if Is_Null (T.The_List) then
         return Opening (T.List_Type) &
                Closing (T.List_Type);
      else
         return Opening (T.List_Type) &
                To_String (Deref (T.The_List).all) &
                Closing (T.List_Type);
      end if;
   end To_Str;


   function New_Lambda_Mal_Type
--     (Left, Right : Smart_Pointer := Smart_Pointers.Null_Smart_pointer)
       (Bin : Binary_Func_Access;
        Rep : Mal_String)
   return Smart_Pointer is
   begin
      return Smart_Pointers.New_Ptr
        (new Lambda_Mal_Type'(Mal_Type with
          Bin => Bin,
          Rep => Ada.Strings.Unbounded.To_Unbounded_String (Rep)));
   end New_Lambda_Mal_Type;

   overriding function Sym_Type (T : Lambda_Mal_Type) return Sym_Types is
   begin
      return Lambda;
   end Sym_Type;

   overriding function To_Str (T : Lambda_Mal_Type) return Mal_String is
   begin
      return "(lambda " & Ada.Strings.Unbounded.To_String (T.Rep) & ")";
   end To_Str;

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


   function Op (A, B : Types.Smart_Pointer) return Types.Smart_Pointer is
      use Types;
      A_Sym_Type : Sym_Types := Deref (A).Sym_Type;
      B_Sym_Type : Sym_Types := Deref (B).Sym_Type;
   begin
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
      else
         return New_Float_Mal_Type
           (Float_Op (Deref_Float (A).Get_Float_Val,
            Deref_Float (B).Get_Float_Val));
      end if;
   end Op;
end Types;
