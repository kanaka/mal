with Ada.Exceptions;
with Ada.Strings.Unbounded;

with Types;
--  We declare a variable of type Types.T.
pragma Elaborate (Types);

package Err is

   --  Error handling.

   --  Built-in function.
   function Throw (Args : in Types.T_Array) return Types.T;

   --  Ada exceptions can only carry an immutable String in each
   --  occurence, so we require a global variable to store the last
   --  exception as a Mal object anyway, and may as well use it for
   --  simple string messages.

   Error : exception;
   Data  : Types.T;
   Trace : Ada.Strings.Unbounded.Unbounded_String;

   --  Convenient shortcuts.

   procedure Raise_With (Message : in String) with No_Return;
   --  Similar to a "raise with Message" Ada statement.
   --  Store the message into Data,
   --  store the message and "Uncaught exception: " into Trace,
   --  then raise Error.

   procedure Raise_In_Mal (E : in Ada.Exceptions.Exception_Occurrence)
     with No_Return;
   --  Raise_With (Ada.Exceptions.Exception_Information (E))

   procedure Add_Trace_Line (Action : in String;
                             Ast    : in Types.T);
   --  Appends a line like "Action: Ast" to Trace.

   procedure Check (Condition : in Boolean;
                    Message   : in String) with Inline;
   --  Raise_With if Condition fails.

   --  It is probably more efficient to construct a boolean and call
   --  this procedure once, as "inline" is only a recommendation.

   --  Beware of the classical performance issue that the Message is
   --  formatted even if the Condition does not hold.

end Err;
