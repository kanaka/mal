with Ada.Text_IO.Unbounded_IO;

with Err;
with Printer;
with Reader;
with Readline;
with Types.Atoms;
with Types.Builtins;
with Types.Fns;
with Types.Mal;
with Types.Maps;
with Types.Sequences;
with Types.Symbols;

procedure Step1_Read_Print is

   use Types;

   function Read return Mal.T_Array with Inline;

   function Eval (Ast : in Mal.T) return Mal.T;

   procedure Print (Ast : in Mal.T) with Inline;

   procedure Rep with Inline;

   ----------------------------------------------------------------------

   function Eval (Ast : in Mal.T) return Mal.T is (Ast);

   procedure Print (Ast : in Mal.T) is
   begin
      Ada.Text_IO.Unbounded_IO.Put_Line (Printer.Pr_Str (Ast));
   end Print;

   function Read return Mal.T_Array
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
   end loop;
   Ada.Text_IO.New_Line;
   --  If assertions are enabled, check deallocations.
   Err.Data := Mal.Nil;  --  Remove references to other packages
   pragma Debug (Atoms.Check_Allocations);
   pragma Debug (Builtins.Check_Allocations);
   pragma Debug (Fns.Check_Allocations);
   pragma Debug (Maps.Check_Allocations);
   pragma Debug (Sequences.Check_Allocations);
   pragma Debug (Symbols.Check_Allocations);
end Step1_Read_Print;
