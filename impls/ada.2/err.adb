with Ada.Characters.Latin_1;

with Printer;
with Types.Strings;

package body Err is

   use Ada.Strings.Unbounded;

   ----------------------------------------------------------------------

   procedure Add_Trace_Line (Action : in String;
                             Ast    : in Types.T)
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

   procedure Raise_In_Mal (E : in Ada.Exceptions.Exception_Occurrence) is
      Message : String renames Ada.Exceptions.Exception_Information (E);
      procedure Process (S : in String);
      procedure Process (S : in String) is
      begin
         Append (Trace, S);
      end Process;
   begin
      Data := (Types.Kind_String, Types.Strings.Alloc (Message));
      Set_Unbounded_String (Trace, "Uncaught exception: ");
      Data.Str.all.Query_Element (Process'Access);
      raise Error;
   end Raise_In_Mal;

   procedure Raise_With (Message : in String) is
   begin
      Data := (Types.Kind_String, Types.Strings.Alloc (Message));
      Set_Unbounded_String (Trace, "Uncaught exception: ");
      Append (Trace, Message);
      Append (Trace, Ada.Characters.Latin_1.LF);
      raise Error;
   end Raise_With;

   function Throw (Args : in Types.T_Array) return Types.T is
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
