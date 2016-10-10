with Ada.Strings.Unbounded;
with Types;

package Printer is

   pragma Elaborate_Body;

   function Pr_Str (Ast            : in Types.Mal_Type;
                    Print_Readably : in Boolean        := True)
                   return Ada.Strings.Unbounded.Unbounded_String;

end Printer;
