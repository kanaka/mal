with Ada.Exceptions;
with Ada.Text_IO.Unbounded_IO;

with Printer;
with Reader;
with Readline;
with Types.Mal;

procedure Step1_Read_Print is

   use Types;

   function Read return Mal.T with Inline;

   function Eval (Ast : in Mal.T) return Mal.T;

   procedure Print (Ast : in Mal.T) with Inline;

   procedure Rep with Inline;

   ----------------------------------------------------------------------

   function Eval (Ast : in Mal.T) return Mal.T is (Ast);

   procedure Print (Ast : in Mal.T) is
   begin
      Ada.Text_IO.Unbounded_IO.Put_Line (Printer.Pr_Str (Ast));
   end Print;

   function Read return Mal.T is (Reader.Read_Str (Readline.Input ("user> ")));

   procedure Rep is
   begin
      Print (Eval (Read));
   end Rep;

   ----------------------------------------------------------------------

begin
   loop
      begin
         Rep;
      exception
         when Readline.End_Of_File =>
            exit;
         when Reader.Empty_Source =>
            null;
         when E : Reader.Reader_Error =>
            Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
         --  Other exceptions are unexpected.
      end;
   end loop;
   Ada.Text_IO.New_Line;
end Step1_Read_Print;
