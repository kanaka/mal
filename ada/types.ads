with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings.Unbounded;

package Types is

   package UBStrings renames Ada.Strings.Unbounded;

   type Mal_Type;

   type Mal_Type_Access is access all Mal_Type;

   package Lists is
     new Ada.Containers.Doubly_Linked_Lists
       (Element_Type => Mal_Type_Access,
        "=" => "=");

   type Sym_Types is (Int, Floating, List, Sym, Str, Atom, Unitary, Error);

   type Unitary_Functions is
     (Quote, Unquote, Quasiquote, Splice_Unquote, Deref);

   type List_Types is (List_List, Vector_List, Hashed_List);
   function Opening (LT : List_Types) return Character;
   function Closing (LT : List_Types) return Character;

   subtype Mal_Float is Float;
   subtype Mal_Integer is Integer;

   type Mal_Type (Sym_Type : Sym_Types) is record
      Meta : Mal_Type_Access;
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
            The_Operand : Mal_Type_Access;
         when Error => Error_Msg : Ada.Strings.Unbounded.Unbounded_String;
      end case;
   end record;

   function To_String (T : Mal_Type) return String;

   procedure Delete_Tree (MTA : in out Mal_Type_Access);

end Types;
