with Ada.Strings.Unbounded;

with Types;

package Printer is

   procedure Pr_Str
     (Buffer   : in out Ada.Strings.Unbounded.Unbounded_String;
      Ast      : in     Types.T;
      Readably : in     Boolean                                := True);
   --  Append the text to Buffer.

   function Pr_Str (Ast      : in Types.T;
                    Readably : in Boolean := True)
                   return Ada.Strings.Unbounded.Unbounded_String;
   --  Return a freshly created unbounded string.
   --  Convenient, but inefficient.

end Printer;
