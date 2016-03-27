with Ada.Text_IO;
with Ada.IO_Exceptions;

procedure Step0_Repl is

   function Read (Param : String) return String is
   begin
      return Param;
   end Read;

   function Eval (Param : String) return String is
   begin
      return Param;
   end Eval;

   function Print (Param : String) return String is
   begin
      return Param;
   end Print;

   function Rep (Param : String) return String is
      Read_Str : String := Read (Param);
      Eval_Str : String := Eval (Read_Str);
      Print_Str : String := Print (Eval_Str);
   begin
      return Print_Str;
   end Rep; 

   S : String (1..1024);
   Last : Natural;

begin

   loop
      Ada.Text_IO.Put ("user> ");
      Ada.Text_IO.Get_Line (S, Last);
      Ada.Text_IO.Put_Line (Rep (S (1..Last)));
   end loop;

exception
   when Ada.IO_Exceptions.End_Error => null;
   -- i.e. exit without textual output
end Step0_Repl;
