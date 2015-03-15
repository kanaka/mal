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

   type Sym_Types is (Int, List, Sym, Str, Atom);

   type Mal_Type (Sym_Type : Sym_Types) is record
      case Sym_Type is
         when Int => Int_Val : Integer;
         when List => The_List : Lists.List;
         when Sym => Symbol : Character;
         when Str => The_String : Ada.Strings.Unbounded.Unbounded_String;
         when Atom => The_Atom : Ada.Strings.Unbounded.Unbounded_String;
      end case;
   end record;

   function To_String (T : Mal_Type) return String;

end Types;
