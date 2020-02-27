with Ada.Text_IO;

with Readline;

procedure Step0_Repl is

   function Read return String with Inline;

   function Eval (Ast : in String) return String;

   procedure Print (Ast : in String) with Inline;

   procedure Rep with Inline;

   ----------------------------------------------------------------------

   function Eval (Ast : in String) return String
   is (Ast);

   procedure Print (Ast : in String) is
   begin
      Ada.Text_IO.Put_Line (Ast);
   end Print;

   function Read return String is (Readline.Input ("user> "));

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
      end;
      --  Other exceptions are really unexpected.
   end loop;
   Ada.Text_IO.New_Line;
end Step0_Repl;
