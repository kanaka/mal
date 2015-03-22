with Ada.Containers.Doubly_Linked_Lists;
with Ada.Finalization;
with Ada.Strings.Unbounded;

package Types is

   package UBStrings renames Ada.Strings.Unbounded;


   -- There's a horrible dependency problem as Smart Pointers are used
   -- within Mal_Type. If you start using private types then the compiler
   -- complains that its being used in Mal_Type before it's been fully
   -- declared.  So that's the reason for all the public types below.
   -- I'm not intending to play with any of the exposed internals...

   -- Alternatively you could use private types and then specify all
   -- the operations including those for lists.  Good luck with that.


   -- Smart Pointers section.

   type Mal_Type;

   type Mal_Type_Accessor is access Mal_Type;

   type Smart_Pointer is new Ada.Finalization.Controlled with record
      Pointer : Mal_Type_Accessor;
   end record;

   overriding procedure Adjust (Object : in out Smart_Pointer);

   overriding procedure Finalize (Object : in out Smart_Pointer);

   function New_Ptr (Mal_Type : Mal_Type_Accessor) return Smart_Pointer;

   function Deref (Ptr : Smart_Pointer) return Mal_Type_Accessor;

   Null_Smart_Pointer : constant Smart_Pointer :=
      (Ada.Finalization.Controlled with Pointer => null);

   -- Lists of Smart Pointers.

   package Lists is
     new Ada.Containers.Doubly_Linked_Lists
       (Element_Type => Smart_Pointer,
        "=" => "=");

   -- Now we get to what the smart pointers are pointing to...

   type Sym_Types is (Int, Floating, List, Sym, Str, Atom, Unitary, Error);

   type Unitary_Functions is
     (Quote, Unquote, Quasiquote, Splice_Unquote, Deref);

   type List_Types is (List_List, Vector_List, Hashed_List);
   function Opening (LT : List_Types) return Character;
   function Closing (LT : List_Types) return Character;

   subtype Mal_Float is Float;
   subtype Mal_Integer is Integer;

   type Mal_Type (Sym_Type : Sym_Types) is record
      Ref_Count : Natural := 1;
      Meta : Smart_Pointer;
      case Sym_Type is
         when Int => Int_Val : Mal_Integer;
         when Floating => Float_Val : Mal_Float;
         when List =>
            List_Type : List_Types;
            The_List : Lists.List;
         when Sym => Symbol : Character;
         when Str => The_String : Ada.Strings.Unbounded.Unbounded_String;
         when Atom => The_Atom : Ada.Strings.Unbounded.Unbounded_String;
         when Unitary =>
            The_Function : Unitary_Functions;
            The_Operand : Smart_Pointer;
         when Error => Error_Msg : Ada.Strings.Unbounded.Unbounded_String;
      end case;
   end record;

   function To_String (T : Mal_Type) return String;

end Types;
