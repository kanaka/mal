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



   function New_List_Mal_Type (List_Type : List_Types)
   return Smart_Pointer is
   begin
      return Smart_Pointers.New_Ptr
        (new List_Mal_Type'(Mal_Type with
          List_Type => List_Type,
          The_List => Smart_Pointers.Null_Smart_Pointer));
   end New_List_Mal_Type;

   overriding function Sym_Type (T : List_Mal_Type) return Sym_Types is
   begin
      return List;
   end Sym_Type;

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


end Types;
