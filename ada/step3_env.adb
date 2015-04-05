with Ada.Command_Line;
with Ada.Text_IO;
with Ada.IO_Exceptions;
with Envs;
with Evaluation;
with Printer;
with Reader;
with Types;

procedure Step3_Env is

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
        Evaluated_AST := Evaluation.Eval (AST);
        return Print (Evaluated_AST);
     end if;

   end Rep; 

   S : String (1..Reader.Max_Line_Len);
   Last : Natural;

begin

   if Ada.Command_Line.Argument_Count > 0 then
     if Ada.Command_Line.Argument (1) = "-d" then
        Evaluation.Debug := True;
     end if;
   end if;

   Envs.New_Env;

   Envs.Set ("+", Types.New_Sym_Mal_Type ('+'));
   Envs.Set ("-", Types.New_Sym_Mal_Type ('-'));
   Envs.Set ("*", Types.New_Sym_Mal_Type ('*'));
   Envs.Set ("/", Types.New_Sym_Mal_Type ('/'));

   loop
      Ada.Text_IO.Put ("user> ");
      Ada.Text_IO.Get_Line (S, Last);
      Ada.Text_IO.Put_Line (Rep (S (1..Last)));
   end loop;

exception
   when Ada.IO_Exceptions.End_Error => null;
   -- i.e. exit without textual output
end Step3_Env;
