with Ada.Strings.Unbounded;

with Types.Mal;

package Printer with Elaborate_Body is

   function Pr_Str (Ast      : in Types.Mal.T;
                    Readably : in Boolean     := True)
                   return Ada.Strings.Unbounded.Unbounded_String;

   function Img (Ast : in Types.Mal.T) return String
   is (Ada.Strings.Unbounded.To_String (Pr_Str (Ast))) with Inline;
   --  This form is convenient for reporting errors, but the
   --  conversion should be avoided when possible.

end Printer;
