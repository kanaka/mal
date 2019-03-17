with Ada.Strings.Unbounded;

with Types.Mal;

--  We declare a variable of type Types.Mal.T.
pragma Elaborate (Types.Mal);

package Err with Elaborate_Body is

   --  Error handling.

   --  Built-in function.
   function Throw (Args : in Types.Mal.T_Array) return Types.Mal.T;

   --  Ada exceptions can only carry an immutable String in each
   --  occurence, so we require a global variable to store the last
   --  exception as a Mal object anyway, and may as well use it for
   --  simple string messages.

   Error : exception;
   Data  : Types.Mal.T;
   Trace : Ada.Strings.Unbounded.Unbounded_String;

   --  Convenient shortcuts.

   procedure Raise_With (Message : in String) with Inline, No_Return;
   --  Similar to a "raise with Message" Ada statement.
   --  Store the message into Data,
   --  store the message and "Uncaught exception: " into Trace,
   --  then raise Error.

   procedure Add_Trace_Line (Action : in String;
                             Ast    : in Types.Mal.T) with Inline;
   --  Appends a line like "Action: Ast" to Trace.

   procedure Check (Condition : in Boolean;
                    Message   : in String) with Inline;
   --  Raise_With if Condition fails.

end Err;
