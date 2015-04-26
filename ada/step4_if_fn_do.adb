with Ada.Command_Line;
with Ada.Exceptions;
with Ada.Text_IO;
with Ada.IO_Exceptions;
with Core;
with Envs;
with Evaluation;
with Printer;
with Reader;
with Types;

procedure Step4_If_Fn_Do is

   function Read (Param : String) return Types.Mal_Handle is
   begin
      return Reader.Read_Str (Param);
   end Read;


   -- Eval can't be here because there are function pointers that point
   -- at it.  Thus it must be at library level.  See evaluation.ads


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
        Evaluated_AST := Evaluation.Eval (AST, Envs.Get_Current);
        return Print (Evaluated_AST);
     end if;

   end Rep; 

   S : String (1..Reader.Max_Line_Len);
   Last : Natural;
   Cmd_Args : Natural;

begin

   Cmd_Args := 0;
   while Ada.Command_Line.Argument_Count > Cmd_Args loop
     Cmd_Args := Cmd_Args + 1;
     if Ada.Command_Line.Argument (Cmd_Args) = "-d" then
        Evaluation.Debug := True;
     elsif Ada.Command_Line.Argument (Cmd_Args) = "-e" then
        Envs.Debug := True;
     end if;
   end loop;

   Core.Init;

   Ada.Text_IO.Put_Line (Rep ("(def! not (fn* (a) (if a false true)))"));

   loop
      begin
         Ada.Text_IO.Put ("user> ");
         Ada.Text_IO.Get_Line (S, Last);
         Ada.Text_IO.Put_Line (Rep (S (1..Last)));
      exception
         when Ada.IO_Exceptions.End_Error => raise;
         when E : others =>
            Ada.Text_IO.Put_Line
              (Ada.Text_IO.Standard_Error,
               Ada.Exceptions.Exception_Information (E));
      end;
   end loop;

exception
   when Ada.IO_Exceptions.End_Error => null;
   -- i.e. exit without textual output
end Step4_If_Fn_Do;
