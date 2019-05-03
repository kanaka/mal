with Ada.Text_IO.Unbounded_IO;

with Err;
with Garbage_Collected;
with Printer;
with Reader;
with Readline;
with Types;

procedure Step1_Read_Print is

   function Read return Types.T_Array with Inline;

   function Eval (Ast : in Types.T) return Types.T;

   procedure Print (Ast : in Types.T) with Inline;

   procedure Rep with Inline;

   ----------------------------------------------------------------------

   function Eval (Ast : in Types.T) return Types.T is (Ast);

   procedure Print (Ast : in Types.T) is
   begin
      Ada.Text_IO.Unbounded_IO.Put_Line (Printer.Pr_Str (Ast));
   end Print;

   function Read return Types.T_Array
   is (Reader.Read_Str (Readline.Input ("user> ")));

   procedure Rep is
   begin
      for Expression of Read loop
         Print (Eval (Expression));
      end loop;
   end Rep;

   ----------------------------------------------------------------------

begin
   loop
      begin
         Rep;
      exception
         when Readline.End_Of_File =>
            exit;
         when Err.Error =>
            Ada.Text_IO.Unbounded_IO.Put (Err.Trace);
      end;
      --  Other exceptions are really unexpected.

      --  Collect garbage.
      Err.Data := Types.Nil;
      --  No data survives at this stage, Repl only contains static
      --  pointers to built-in functions.
      Garbage_Collected.Clean;
   end loop;
   Ada.Text_IO.New_Line;
   --  If assertions are enabled, check deallocations.
   --  Normal runs do not need to deallocate before termination.
   --  Beware that all pointers are now dangling.
   pragma Debug (Garbage_Collected.Clean);
   Garbage_Collected.Check_Allocations;
end Step1_Read_Print;
