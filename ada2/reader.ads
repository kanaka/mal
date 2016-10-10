with Types;

package Reader is

   pragma Elaborate_Body;

   function Read_Str (Source : in String) return Types.Mal_Type;

   Empty_Source : exception;
   Reader_Error : exception;

end Reader;
