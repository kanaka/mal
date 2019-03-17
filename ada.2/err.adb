with Ada.Characters.Latin_1;

with Printer;

package body Err is

   use Ada.Strings.Unbounded;
   use Types;

   ----------------------------------------------------------------------

   procedure Add_Trace_Line (Action : in String;
                             Ast    : in Types.Mal.T)
   is
   begin
      Append (Trace, "  in ");
      Append (Trace, Action);
      Append (Trace, ": ");
      Printer.Pr_Str (Trace, Ast);
      Append (Trace, Ada.Characters.Latin_1.LF);
   end Add_Trace_Line;

   procedure Check (Condition : in Boolean;
                    Message   : in String)
   is
   begin
      if not Condition then
         Raise_With (Message);
      end if;
   end Check;

   procedure Raise_With (Message : in String) is
   begin
      Data := (Kind_String, To_Unbounded_String (Message));
      Set_Unbounded_String (Trace, "Uncaught exception: ");
      Append (Trace, Message);
      Append (Trace, Ada.Characters.Latin_1.LF);
      raise Error;
   end Raise_With;

   function Throw (Args : in Mal.T_Array) return Mal.T is
   begin
      Check (Args'Length = 1, "expected 1 parameter");
      Data := Args (Args'First);
      Set_Unbounded_String (Trace, "Uncaught exception: ");
      Printer.Pr_Str (Trace, Data);
      Append (Trace, Ada.Characters.Latin_1.LF);
      --  A raise value is equivalent to a raise statement, but
      --  silents a compiler warning.
      return raise Error;
   end Throw;

end Err;
