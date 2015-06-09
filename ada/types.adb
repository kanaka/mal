with Ada.Characters.Latin_1;
with Ada.Strings.Fixed;
with Ada.Strings.Maps.Constants;
with Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with Envs;
with Smart_Pointers;

package body Types is

   package ACL renames Ada.Characters.Latin_1;


   function "=" (A, B : Mal_Handle) return Mal_Handle is
   begin
      return New_Bool_Mal_Type (A = B);
   end "=";


   function "=" (A, B : Mal_Handle) return Boolean is
   begin

      if (not Is_Null (A) and not Is_Null (B)) and then
         Deref (A).Sym_Type = Deref (B).Sym_Type then

         case Deref (A).Sym_Type is
            when Int =>
               return (Deref_Int (A).Get_Int_Val = Deref_Int (B).Get_Int_Val);
            when Floating =>
               return (Deref_Float (A).Get_Float_Val = Deref_Float (B).Get_Float_Val);
            when Bool =>
               return (Deref_Bool (A).Get_Bool = Deref_Bool (B).Get_Bool);
            when List =>
               return (Deref_List (A).all = Deref_List (B).all);
            when Str =>
               return (Deref_String (A).Get_String = Deref_String (B).Get_String);
            when Atom =>
               return (Deref_Atom (A).Get_Atom = Deref_Atom (B).Get_Atom);
            when Func =>
               return (Deref_Func (A).Get_Func_Name = Deref_Func (B).Get_Func_Name);
            when Unitary =>
               return (Deref_Int(A).Get_Int_Val = Deref_Int(B).Get_Int_Val);
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
       return T.Meta;
   end Get_Meta;

   procedure Set_Meta (T : in out Mal_Type'Class; SP : Mal_Handle) is
   begin
      T.Meta := SP;
   end Set_Meta;

   function To_String (T : Mal_Type'Class; Print_Readably : Boolean := True)
   return Mal_String is
   begin
      if not Is_Null (T.Meta) then
         return "(with-meta " &
                To_Str (T, Print_Readably) & " " &
                To_Str (Deref (T.Meta).all, Print_Readably) & ")";
      else
         return To_Str (T, Print_Readably);
      end if;
   end To_String;


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
      I : Positive := 2;
      Str_Len : Natural;
      Res : Unbounded_String;
   begin
      if Print_Readably then
         Append (Res, '"');
         Str_Len := Length (T.The_String);
         while I < Str_Len loop
            if Element (T.The_String, I) = '"' then
               Append (Res, "\""");
            elsif Element (T.The_String, I) = '\' then
               Append (Res, "\\");
            elsif Element (T.The_String, I) = Ada.Characters.Latin_1.LF then
               Append (Res, "\n");
            else
               Append (Res, Element (T.The_String, I));
            end if;
            I := I + 1;
         end loop;
         Append (Res, '"');
         return Ada.Strings.Unbounded.To_String (Res);
      else
         return Slice (T.The_String, 2, Length (T.The_String) - 1);
      end if;
   end To_Str;


   function New_Atom_Mal_Type (Str : Mal_String) return Mal_Handle is
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

   function Deref_Atom (S : Mal_Handle) return Atom_Ptr is
   begin
      return Atom_Ptr (Deref (S));
   end Deref_Atom;

   overriding function To_Str 
     (T : Atom_Mal_Type; Print_Readably : Boolean := True)
   return Mal_String is
   begin
      return Ada.Strings.Unbounded.To_String (T.The_Atom);
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
     (FMT : Func_Mal_Type; Rest_List : Mal_Handle; Env : Envs.Env_Handle)
   return Mal_Handle is
   begin
      return FMT.Func_P (Rest_List, Env);
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


   function New_Unitary_Mal_Type (Func : Unitary_Functions; Op : Mal_Handle)
   return Mal_Handle is
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

   function Get_Op (T : Unitary_Mal_Type) return Mal_Handle is
   begin
     return T.The_Operand;
   end Get_Op;

   function Map_Nodes
     (Func_Ptr : Func_Access;
      L : Node_Mal_Type)
   return Mal_Handle is
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
   return Mal_Handle is
      C_Node : Node_Mal_Type := L;
      Res : Mal_Handle;
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


   overriding function To_Str 
     (T : Unitary_Mal_Type; Print_Readably : Boolean := True)
   return Mal_String is
   begin
      case T.The_Function is
         when Quote =>
            return "(quote " & To_String (Deref (T.The_Operand).all, True) & ")";
         when Unquote =>
            return "(unquote " & To_String (Deref (T.The_Operand).all, Print_Readably) & ")";
         when Quasiquote =>
            return "(quasiquote " & To_String (Deref (T.The_Operand).all, Print_Readably) & ")";
         when Splice_Unquote =>
            return
              "(splice-unquote " & To_String (Deref (T.The_Operand).all, Print_Readably) & ")";
         when Deref =>
            return
              "(deref " & To_String (Deref (T.The_Operand).all, Print_Readably) & ")";
      end case;
   end To_Str;


   function Nodes_Equal (A, B : Mal_Handle) return Boolean is
   begin
      if (not Is_Null (A) and not Is_Null (B)) and then
         Deref (A).Sym_Type = Deref (B).Sym_Type then
         if Deref (A).Sym_Type = Node then
            return
              Nodes_Equal (Deref_Node (A).Left, Deref_Node (B).Left) and then
              Nodes_Equal (Deref_Node (A).Right, Deref_Node (B).Right);
         else
            return A = B; 
         end if;
      elsif Is_Null (A) and Is_Null (B) then
         return True;
      else  -- either one of the args is null or the sym_types don't match
         return False;
      end if;
   end Nodes_Equal;

   function New_Node_Mal_Type (Left, Right : Mal_Handle :=
                        Smart_Pointers.Null_Smart_pointer)
   return Mal_Handle is
   begin
      return Smart_Pointers.New_Ptr
        (new Node_Mal_Type' (Mal_Type with Left => Left, Right => Right));
   end New_Node_Mal_Type;

   overriding function Sym_Type (T : Node_Mal_Type) return Sym_Types is
   begin
      return Node;
   end Sym_Type;

   procedure Append (To_List : in out Node_Mal_Type; Op : Mal_Handle) is
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
         To_List.Right := New_Node_Mal_Type
            (Left => To_List.Right,
             Right => Op);
      end if;
   end Append;

   function Node_Length (L : Mal_Handle) return Natural is
      Right  : Mal_Handle;
   begin
      if Is_Null (L) then
         return 0;
      else
         Right := Deref_Node (L).Right;
         if Is_Null (Right) then
            -- Its a node; there must be something in the Left, right? ;)
            return 1;
         elsif Deref (Right).Sym_Type = Node then
            -- Right is a node so recurse but +1 for the Left just passed.
            return Node_Length (Right) + 1;
         else
            -- Right is not null but not node.
            return 2;
         end if;
      end if;
   end Node_Length;

   -- Get the first item in the list:
   function Car (L : List_Mal_Type) return Mal_Handle is
   begin
      if Is_Null (L.The_List) then
         return Smart_Pointers.Null_Smart_Pointer;
      else
         return Deref_Node (L.The_List).Left;
      end if;
   end Car;
   
   
   -- Get the rest of the list (second item onwards)
   function Cdr (L : List_Mal_Type) return Mal_Handle is
   begin
      if Is_Null (L.The_List) or else
         Is_Null (Deref_Node (L.The_List).Right) then
         return New_List_Mal_Type (L.List_Type);
      end if;
      declare
         Node_P : Node_Ptr;
      begin 
         Node_P := Deref_Node (L.The_List);
         -- Clojure lists are constants?
         -- If not, need to copy P.Right to a new list...
         -- Or maybe we copy on write?
         if Deref (Node_P.Right).Sym_Type = Node then
            return New_List_Mal_Type (L.List_Type, Node_P.Right);
         else
            -- Right is not a Node! We'd better make one.
            return New_List_Mal_Type
                     (L.List_Type,
                      New_Node_Mal_Type (Left => Node_P.Right));
         end if;
      end;
   end Cdr;

   function Length (L : List_Mal_Type) return Natural is
   begin
      return Node_Length (L.The_List);
   end Length;

   function Is_Null (L : List_Mal_Type) return Boolean is
      use Smart_Pointers;
   begin
      return Smart_Pointers."="(L.The_List, Null_Smart_Pointer);
   end Is_Null;

   function Null_List (L : List_Types) return List_Mal_Type is
   begin
      return (Mal_Type with List_Type => L,
              The_List => Smart_Pointers.Null_Smart_Pointer);
   end Null_List;


   function Map
     (Func_Ptr : Func_Access;
      L : List_Mal_Type)
   return Mal_Handle is
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
   return Mal_Handle is
   begin
      if Is_Null (L.The_List) then
         return New_List_Mal_Type (L.Get_List_Type);
      else
         return Reduce_Nodes (Func_Ptr, Deref_Node (L.The_List).all);
      end if;
   end Reduce;

   overriding function To_Str 
     (T : Node_Mal_Type; Print_Readably : Boolean := True)
   return Mal_String is
   begin
      if Is_Null (T.Left) then
         -- Left is null and by implication so is right.
         return "";
      elsif Is_Null (T.Right) then
        -- Left is not null but right is.
        return To_Str (Deref (T.Left).all, Print_Readably);
      else
        -- Left and right are both not null.
        return To_Str (Deref (T.Left).all, Print_Readably) &
               " " &
               To_Str (Deref (T.Right).all, Print_Readably);
      end if;
   end To_Str;

   function Cat_Str (T : Node_Mal_Type; Print_Readably : Boolean := True) return Mal_String is
   begin
      if Is_Null (T.Left) then
         -- Left is null and by implication so is right.
         return "";
      elsif Is_Null (T.Right) then
        -- Left is not null but right is.
        return To_Str (Deref (T.Left).all, Print_Readably);

      -- Left and right are both not null.
      elsif Deref (T.Right).Sym_Type = Node then
        return To_Str (Deref (T.Left).all, Print_Readably) &
               Cat_Str (Deref_Node (T.Right).all, Print_Readably);
      else
        return To_Str (Deref (T.Left).all, Print_Readably) &
               To_Str (Deref (T.Right).all, Print_Readably);
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
          The_List => The_List.The_List));
   end New_List_Mal_Type;


   function New_List_Mal_Type
     (List_Type : List_Types;
      The_First_Node : Mal_Handle := Smart_Pointers.Null_Smart_Pointer)
   return Mal_Handle is
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

   function Prepend (Op : Mal_Handle; To_List : List_Mal_Type)
   return Mal_Handle is
   begin
      return New_List_Mal_Type
               (To_List.Get_List_Type,
                New_Node_Mal_Type (Op, To_List.The_List));
   end Prepend;

   procedure Append (To_List : in out List_Mal_Type; Op : Mal_Handle) is
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

   function Deref_List (SP : Mal_Handle) return List_Ptr is
   begin
      return List_Ptr (Deref (SP));
   end Deref_List;


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
     (Params : Mal_Handle; Expr : Mal_Handle)
   return Mal_Handle is
   begin
      return Smart_Pointers.New_Ptr
        (new Lambda_Mal_Type'
          (Mal_Type with Env => Envs.Get_Current,
           Params => Params,
           Expr => Expr));
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
      return L.Params;
   end Get_Params;

   function Get_Expr (L : Lambda_Mal_Type) return Mal_Handle is
   begin
      return L.Expr;
   end Get_Expr;

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
