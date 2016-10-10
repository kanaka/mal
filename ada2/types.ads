with Atoms;
with Environments;
with Lists;
with Maps;
with Strings;

package Types is

   type Mal_Type;
   type Mal_Type_Array;
   type Native_Function_Access is not null access
     function (Arguments : in Mal_Type_Array) return Mal_Type;

   --  Make similar kinds consecutive for efficient case statements.
   type Kind_Type is
     (Kind_Nil,
      Kind_Atom,
      Kind_Boolean,
      Kind_Number,
      Kind_String, Kind_Symbol, Kind_Keyword,
      Kind_List, Kind_Vector,
      Kind_Map,
      Kind_Macro, Kind_Function, Kind_Native);

   type Mal_Type (Kind : Kind_Type := Kind_Nil) is record
      Meta : Atoms.Ptr;
      case Kind is
         when Kind_Nil =>
            null;
         when Kind_Boolean =>
            Boolean_Value             : Boolean;
         when Kind_Number =>
            Integer_Value             : Integer;
         when Kind_Atom =>
            Reference                 : Atoms.Ptr;
         when Kind_String | Kind_Keyword | Kind_Symbol =>
            S                         : Strings.Ptr;
         when Kind_List | Kind_Vector =>
            L                         : Lists.Ptr;
         when Kind_Map =>
            Map                       : Maps.Ptr;
         when Kind_Native =>
            Native                    : Native_Function_Access;
         when Kind_Function =>
            Formals                   : Lists.Ptr;
            Expression                : Atoms.Ptr;
            Environment               : Environments.Ptr;
         when Kind_Macro =>
            Mac_Formals               : Lists.Ptr;
            Mac_Expression            : Atoms.Ptr;
      end case;
   end record;

   function "=" (Left, Right : in Mal_Type) return Boolean;
   --  By default, a list /= a vector.

   type Mal_Type_Array is array (Positive range <>) of Types.Mal_Type;

end Types;
