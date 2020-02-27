with Ada.Text_IO;
with Printer;
with Reader;
with Types;

procedure Step1_Read_Print is

   function Read (Param : String) return Types.Mal_Handle is
   begin
      return Reader.Read_Str (Param);
   end Read;

   function Eval (Param : Types.Mal_Handle) return Types.Mal_Handle is
   begin
      return Param;
   end Eval;

   function Print (Param : Types.Mal_Handle) return String is
   begin
      return Printer.Pr_Str (Param);
   end Print;

   function Rep (Param : String) return String is
      AST, Evaluated_AST : Types.Mal_Handle;
   begin

      AST := Read (Param);

      if Types.Is_Null (AST) then
         return "";
      else
         Evaluated_AST := Eval (AST);
         return Print (Evaluated_AST);
      end if;

   end Rep;

begin
   loop
      Ada.Text_IO.Put ("user> ");
      exit when Ada.Text_IO.End_Of_File;
      Ada.Text_IO.Put_Line (Rep (Ada.Text_IO.Get_Line));
   end loop;
end Step1_Read_Print;
