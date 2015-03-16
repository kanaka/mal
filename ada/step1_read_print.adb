with Ada.Text_IO;
with Ada.IO_Exceptions;
with Printer;
with Reader;
with Types;

procedure Step1_Read_Print is

   function Read (Param : String) return Types.Mal_Type_Access is
   begin
      return Reader.Read_Str (Param);
   end Read;

   function Eval (Param : Types.Mal_Type_Access) return Types.Mal_Type_Access is
   begin
      return Param;
   end Eval;

   function Print (Param : Types.Mal_Type_Access) return String is
   begin
      return Printer.Pr_Str (Param);
   end Print;

   function Rep (Param : String) return String is
     AST : Types.Mal_Type_Access := Read (Param);
     Eval_Str : Types.Mal_Type_Access := Eval (AST);
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
      Ada.Text_IO.Put_Line (Rep (S(1..Last)));
   end loop;

exception
   when Ada.IO_Exceptions.End_Error => null;
   -- i.e. exit without textual output
end Step1_Read_Print;
