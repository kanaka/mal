with Ada.Strings.Unbounded;

with Types.Mal;

package Printer with Elaborate_Body is

   function Pr_Str (Ast      : in Types.Mal.T;
                    Readably : in Boolean     := True)
                   return Ada.Strings.Unbounded.Unbounded_String;

end Printer;
