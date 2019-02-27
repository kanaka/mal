with Types.Mal;

package Reader with Elaborate_Body is

   function Read_Str (Source : in String) return Types.Mal.T;

   Empty_Source : exception;
   Reader_Error : exception;

end Reader;
