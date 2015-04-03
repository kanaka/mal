with Types;

package Reader is

   Max_Line_Len : constant := 1024;

   -- This is the Parser (returns an AST)
   function Read_Str (S : String) return Types.Mal_Handle;
   
end Reader;
