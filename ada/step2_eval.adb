with Ada.Text_IO;
with Ada.IO_Exceptions;
with Envs;
with Evaluation;
with Printer;
with Reader;
with Types;

procedure Step2_Eval is

   function Read (Param : String) return Types.Smart_Pointer is
   begin
      return Reader.Read_Str (Param);
   end Read;


   -- Eval can't be here because there are function pointers that point
   -- at it.  Thus it must be at library level.  See evaluation.ads


   function Print (Param : Types.Smart_Pointer) return String is
   begin
      return Printer.Pr_Str (Param);
   end Print;

   function Rep (Param : String) return String is
     AST, Evaluated_AST : Types.Smart_Pointer;
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

   Envs.New_Env;

   Envs.Add ("+", Types.New_Sym_Mal_Type ('+'));
   Envs.Add ("-", Types.New_Sym_Mal_Type ('-'));
   Envs.Add ("*", Types.New_Sym_Mal_Type ('*'));
   Envs.Add ("/", Types.New_Sym_Mal_Type ('/'));

   loop
      Ada.Text_IO.Put ("user> ");
      Ada.Text_IO.Get_Line (S, Last);
      Ada.Text_IO.Put_Line (Rep (S (1..Last)));
   end loop;

exception
   when Ada.IO_Exceptions.End_Error => null;
   -- i.e. exit without textual output
end Step2_Eval;
