with Ada.Text_IO;

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

begin
   loop
      Ada.Text_IO.Put ("user> ");
      exit when Ada.Text_IO.End_Of_File;
      Ada.Text_IO.Put_Line (Rep (Ada.Text_IO.Get_Line));
   end loop;
end Step0_Repl;
