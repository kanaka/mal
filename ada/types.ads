-- This started out as a simple public variant record.
-- Then smart pointers were added.  They were part of the Mal_Type and
-- were required to be public because of the dependencies and
-- how the variant record was public.  Not very Ada-like.
-- The third version bites the bullet and delares Mal_Type as tagged.
-- Smart pointers are an OO version in a separate package.
-- The Doubly_Linked_Lists have been replaced with a tree-like list instead...
-- The tree-like list has been replaced with a singly linked list. Sigh.

-- WARNING!  This code contains:
-- Recursive data structures.
-- Object-based smart pointers.
-- Object-oriented code.
-- And strong-typing!

-- Chris M Moore 25/03/2015

with Ada.Strings.Unbounded;
with Smart_Pointers;
with Envs;

package Types is

   -- Some simple types.  Not supposed to use the standard types directly.

   subtype Mal_Float is Float;
   subtype Mal_Integer is Integer;
   subtype Mal_String is String;

   -- Start off with the top-level abstract type.

   subtype Mal_Handle is Smart_Pointers.Smart_Pointer;

   function "=" (A, B : Mal_Handle) return Mal_Handle;

   function "=" (A, B : Mal_Handle) return Boolean;

   type Sym_Types is (Nil, Bool, Int, Floating, Str, Sym, Atom, Node,
                      List, Func, Lambda, Error);

   type Mal_Type is abstract new Smart_Pointers.Base_Class with private;

   function Sym_Type (T : Mal_Type) return Sym_Types is abstract;

   function Get_Meta (T : Mal_Type) return Mal_Handle;

   procedure Set_Meta (T : in out Mal_Type'Class; SP : Mal_Handle);

   function Copy (M : Mal_Handle) return Mal_Handle;

   function To_String (T : Mal_Type'Class; Print_Readably : Boolean := True)
   return Mal_String;

   function Is_Macro_Call (T : Mal_Type'Class; Env : Envs.Env_Handle) return Boolean;

   type Mal_Ptr is access all Mal_Type'Class;

   -- A helper function that just view converts the smart pointer to
   -- a Mal_Type'Class pointer.
   function Deref (S : Mal_Handle) return Mal_Ptr;

   -- A helper function to detect null smart pointers.
   function Is_Null (S : Mal_Handle) return Boolean;

   -- Derived types.  All boilerplate from here.

   type Nil_Mal_Type is new Mal_Type with private;

   function New_Nil_Mal_Type return Mal_Handle;

   overriding function Sym_Type (T : Nil_Mal_Type) return Sym_Types;


   type Int_Mal_Type is new Mal_Type with private;

   function New_Int_Mal_Type (Int : Mal_Integer) return Mal_Handle;

   overriding function Sym_Type (T : Int_Mal_Type) return Sym_Types;

   function Get_Int_Val (T : Int_Mal_Type) return Mal_Integer;

   type Int_Ptr is access all Int_Mal_Type;

   function Deref_Int (SP : Mal_Handle) return Int_Ptr;


   type Float_Mal_Type is new Mal_Type with private;

   function New_Float_Mal_Type (Floating : Mal_Float) return Mal_Handle;

   overriding function Sym_Type (T : Float_Mal_Type) return Sym_Types;

   function Get_Float_Val (T : Float_Mal_Type) return Mal_Float;

   type Float_Ptr is access all Float_Mal_Type;

   function Deref_Float (SP : Mal_Handle) return Float_Ptr;


   type Bool_Mal_Type is new Mal_Type with private;

   function New_Bool_Mal_Type (Bool : Boolean) return Mal_Handle;

   overriding function Sym_Type (T : Bool_Mal_Type) return Sym_Types;

   function Get_Bool (T : Bool_Mal_Type) return Boolean;

   type Bool_Ptr is access all Bool_Mal_Type;

   function Deref_Bool (SP : Mal_Handle) return Bool_Ptr;


   type String_Mal_Type is new Mal_Type with private;

   function New_String_Mal_Type (Str : Mal_String) return Mal_Handle;

   overriding function Sym_Type (T : String_Mal_Type) return Sym_Types;

   function Get_String (T : String_Mal_Type) return Mal_String;

   type String_Ptr is access all String_Mal_Type;

   function Deref_String (SP : Mal_Handle) return String_Ptr;


   type Symbol_Mal_Type is new Mal_Type with private;

   function New_Symbol_Mal_Type (Str : Mal_String) return Mal_Handle;

   overriding function Sym_Type (T : Symbol_Mal_Type) return Sym_Types;

   function Get_Sym (T : Symbol_Mal_Type) return Mal_String;

   type Sym_Ptr is access all Symbol_Mal_Type;

   function Deref_Sym (S : Mal_Handle) return Sym_Ptr;



   type Atom_Mal_Type is new Mal_Type with private;

   function New_Atom_Mal_Type (MH : Mal_Handle) return Mal_Handle;

   overriding function Sym_Type (T : Atom_Mal_Type) return Sym_Types;

   function Get_Atom (T : Atom_Mal_Type) return Mal_Handle;

   procedure Set_Atom (T : in out Atom_Mal_Type; New_Val : Mal_Handle);

   type Atom_Ptr is access all Atom_Mal_Type;

   function Deref_Atom (S : Mal_Handle) return Atom_Ptr;



   type Error_Mal_Type is new Mal_Type with private;

   function New_Error_Mal_Type (Str : Mal_String) return Mal_Handle;

   overriding function Sym_Type (T : Error_Mal_Type) return Sym_Types;


   -- Lists.

   type List_Types is (List_List, Vector_List, Hashed_List);
   function Opening (LT : List_Types) return Character;
   function Closing (LT : List_Types) return Character;

   type List_Mal_Type is new Mal_Type with private;

   function "=" (A, B : List_Mal_Type) return Boolean;

   function New_List_Mal_Type
     (List_Type : List_Types;
      The_First_Node : Mal_Handle := Smart_Pointers.Null_Smart_Pointer)
   return Mal_Handle;

   function New_List_Mal_Type
     (The_List : List_Mal_Type)
   return Mal_Handle;

   type Handle_Lists is array (Positive range <>) of Mal_Handle;

   -- Make a new list of the form: (Handle_List(1), Handle_List(2)...)
   function Make_New_List (Handle_List : Handle_Lists) return Mal_Handle;

   overriding function Sym_Type (T : List_Mal_Type) return Sym_Types;

   function Get_List_Type (L : List_Mal_Type) return List_Types;

   function Prepend (Op : Mal_Handle; To_List : List_Mal_Type)
   return Mal_Handle;

   procedure Append (To_List : in out List_Mal_Type; Op : Mal_Handle);

   function Length (L : List_Mal_Type) return Natural;

   function Nth (L : List_Mal_Type; N : Natural) return Mal_Handle;

   procedure Add_Defs (Defs : List_Mal_Type; Env : Envs.Env_Handle);

   -- Get the first item in the list:
   function Car (L : List_Mal_Type) return Mal_Handle;

   -- Get the rest of the list (second item onwards)
   function Cdr (L : List_Mal_Type) return Mal_Handle;

   type Func_Access is access
     function (Elem : Mal_Handle)
     return Mal_Handle;

   function Map
     (Func_Ptr : Func_Access;
      L : List_Mal_Type)
   return Mal_Handle;

   type Binary_Func_Access is access
     function (A, B : Mal_Handle)
     return Mal_Handle;

   function Reduce
     (Func_Ptr : Binary_Func_Access;
      L : List_Mal_Type)
   return Mal_Handle;

   function Is_Null (L : List_Mal_Type) return Boolean;

   function Null_List (L : List_Types) return List_Mal_Type;

   function Pr_Str (T : List_Mal_Type; Print_Readably : Boolean := True)
   return Mal_String;

   function Cat_Str (T : List_Mal_Type; Print_Readably : Boolean := True)
   return Mal_String;

   function Concat (Rest_Handle : List_Mal_Type)
   return Types.Mal_Handle;  -- a new list

   -- Duplicate copies the list (logically).  This is to allow concatenation,
   -- The result is always a List_List.
   function Duplicate (The_List : List_Mal_Type) return Mal_Handle;

   type List_Ptr is access all List_Mal_Type;

   function Deref_List (SP : Mal_Handle) return List_Ptr;

   type List_Class_Ptr is access all List_Mal_Type'Class;

   function Deref_List_Class (SP : Mal_Handle) return List_Class_Ptr;


   type Func_Mal_Type is new Mal_Type with private;

   type Builtin_Func is access
      function (MH : Mal_Handle) return Mal_Handle;

   function New_Func_Mal_Type (Str : Mal_String; F : Builtin_Func)
   return Mal_Handle;

   overriding function Sym_Type (T : Func_Mal_Type) return Sym_Types;

   function Get_Func_Name (T : Func_Mal_Type) return Mal_String;

   function Call_Func
     (FMT : Func_Mal_Type; Rest_List : Mal_Handle)
   return Mal_Handle;

   type Func_Ptr is access all Func_Mal_Type;

   function Deref_Func (S : Mal_Handle) return Func_Ptr;



   type Lambda_Mal_Type is new Mal_Type with private;

   function New_Lambda_Mal_Type
     (Params : Mal_Handle; Expr : Mal_Handle; Env : Envs.Env_Handle)
   return Mal_Handle;

   overriding function Sym_Type (T : Lambda_Mal_Type) return Sym_Types;

   function Get_Env (L : Lambda_Mal_Type) return Envs.Env_Handle;

   procedure Set_Env (L : in out Lambda_Mal_Type; Env : Envs.Env_Handle);

   function Get_Params (L : Lambda_Mal_Type) return Mal_Handle;

   function Get_Expr (L : Lambda_Mal_Type) return Mal_Handle;

   function Get_Is_Macro (L : Lambda_Mal_Type) return Boolean;

   procedure Set_Is_Macro (L : in out Lambda_Mal_Type; B : Boolean);

   function Apply
     (L : Lambda_Mal_Type;
      Param_List : Mal_Handle) return Mal_Handle;

   type Lambda_Ptr is access all Lambda_Mal_Type;

   function Get_Macro (T : Mal_Handle; Env : Envs.Env_Handle) return Lambda_Ptr;

   function Deref_Lambda (SP : Mal_Handle) return Lambda_Ptr;

   generic
      with function Int_Op (A, B : Mal_Integer) return Mal_Integer;
      with function Float_Op (A, B : Mal_Float) return Mal_Float;
   function Arith_Op (A, B : Mal_Handle) return Mal_Handle;

   generic
      with function Int_Rel_Op (A, B : Mal_Integer) return Boolean;
      with function Float_Rel_Op (A, B : Mal_Float) return Boolean;
   function Rel_Op (A, B : Mal_Handle) return Mal_Handle;

   Mal_Exception : exception;  -- So tempting to call this Mal_Function but...

   Mal_Exception_Value : Mal_Handle;  -- Used by mal's throw command

private

   type Mal_Type is abstract new Smart_Pointers.Base_Class with record
      Meta : Mal_Handle;
   end record;

   -- Not allowed to be abstract and private.  RM 3.9.3(10)
   -- So if you call this it'll just raise an exception.
   function To_Str (T : Mal_Type; Print_Readably : Boolean := True)
   return Mal_String;

   type Nil_Mal_Type is new Mal_Type with null record;

   overriding function To_Str (T : Nil_Mal_Type; Print_Readably : Boolean := True)
   return Mal_String;

   type Int_Mal_Type is new Mal_Type with record
      Int_Val : Mal_Integer;
   end record;

   overriding function To_Str (T : Int_Mal_Type; Print_Readably : Boolean := True)
   return Mal_String;

   type Float_Mal_Type is new Mal_Type with record
      Float_Val : Mal_Float;
   end record;

   overriding function To_Str (T : Float_Mal_Type; Print_Readably : Boolean := True)
   return Mal_String;

   type Bool_Mal_Type is new Mal_Type with record
      Bool_Val : Boolean;
   end record;

   overriding function To_Str (T : Bool_Mal_Type; Print_Readably : Boolean := True)
   return Mal_String;

   type String_Mal_Type is new Mal_Type with record
      The_String : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   overriding function To_Str (T : String_Mal_Type; Print_Readably : Boolean := True)
   return Mal_String;

   type Symbol_Mal_Type is new Mal_Type with record
      The_Symbol : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   overriding function To_Str (T : Symbol_Mal_Type; Print_Readably : Boolean := True)
   return Mal_String;

   type Atom_Mal_Type is new Mal_Type with record
      The_Atom : Mal_Handle;
   end record;

   overriding function To_Str (T : Atom_Mal_Type; Print_Readably : Boolean := True)
   return Mal_String;

   type Func_Mal_Type is new Mal_Type with record
      Func_Name : Ada.Strings.Unbounded.Unbounded_String;
      Func_P : Builtin_Func;
   end record;

   overriding function To_Str (T : Func_Mal_Type; Print_Readably : Boolean := True)
   return Mal_String;

   type Error_Mal_Type is new Mal_Type with record
      Error_Msg : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   overriding function To_Str (T : Error_Mal_Type; Print_Readably : Boolean := True)
   return Mal_String;


   -- Nodes have to be a differnt type from a List;
   -- otherwise how do you represent a list within a list?
   type Node_Mal_Type is new Mal_Type with record
      Data : Mal_Handle;
      Next : Mal_Handle;  -- This is always a Node_Mal_Type handle
   end record;

   function New_Node_Mal_Type
     (Data : Mal_Handle;
      Next : Mal_Handle := Smart_Pointers.Null_Smart_Pointer)
   return Mal_Handle;

   overriding function Sym_Type (T : Node_Mal_Type) return Sym_Types;

   overriding function To_Str 
     (T : Node_Mal_Type; Print_Readably : Boolean := True)
   return Mal_String;

   type Node_Ptr is access all Node_Mal_Type;

   function Deref_Node (SP : Mal_Handle) return Node_Ptr;


   type List_Mal_Type is new Mal_Type with record
      List_Type : List_Types;
      The_List : Mal_Handle;
      Last_Elem : Mal_Handle;
   end record;

   overriding function To_Str 
     (T : List_Mal_Type; Print_Readably : Boolean := True)
   return Mal_String;

   type Container_Cursor is tagged record
      The_Node : Node_Ptr := null;
   end record;

   type Lambda_Mal_Type is new Mal_Type with record
      Params, Expr : Mal_Handle;
      Env : Envs.Env_Handle;
      Is_Macro : Boolean;
   end record;

   overriding function To_Str 
     (T : Lambda_Mal_Type; Print_Readably : Boolean := True)
   return Mal_String;


end Types;
