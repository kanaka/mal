with Types.Mal;

package Reader with Elaborate_Body is

   function Read_Str (Source : in String) return Types.Mal.T_Array;
   --  The language does not explicitly define what happens when the
   --  input string contains more than one expression.
   --  This implementation returns all of them.

end Reader;
