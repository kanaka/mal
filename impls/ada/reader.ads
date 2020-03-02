with Types;

package Reader is

   -- This is the Parser (returns an AST)
   function Read_Str (S : String) return Types.Mal_Handle;

private

   procedure Lex_Init (S : String);

   function Read_Form return Types.Mal_Handle;

end Reader;
