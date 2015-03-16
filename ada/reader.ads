with Types;

package Reader is

   -- This is the Parser (returns an AST)
   function Read_Str (S : String) return Types.Mal_Type_Access;
   
end Reader;
