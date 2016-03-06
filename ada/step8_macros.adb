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

procedure Step8_Macros is

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
   Cmd_Args, File_Param : Natural;
   Command_Args : Types.Mal_Handle;
   Command_List : Types.List_Ptr;
   File_Processed : Boolean := False;

begin

   -- Core init also creates the first environment.
   -- This is needed for the def!'s below.
   Core.Init;

   declare
      Not_S : String :=
        Rep ("(def! not (fn* (a) (if a false true)))");
      LF_S : String :=
        Rep ("(def! load-file (fn* (f) (eval (read-string (str ""(do "" (slurp f) "")"")))))");
      Cond_S : String :=
        Rep ("(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw ""odd number of forms to cond"")) (cons 'cond (rest (rest xs)))))))");
      Or_S : String :=
        Rep ("(defmacro! or (fn* (& xs) (if (empty? xs) nil (if (= 1 (count xs)) (first xs) `(let* (or_FIXME ~(first xs)) (if or_FIXME or_FIXME (or ~@(rest xs))))))))");
      pragma Unreferenced (Not_S, LF_S, Cond_S, Or_S);
   begin
      null;
   end;

   Cmd_Args := 0;
   Command_Args := Types.New_List_Mal_Type (Types.List_List);
   Command_List := Types.Deref_List (Command_Args);

   while Ada.Command_Line.Argument_Count > Cmd_Args loop

     Cmd_Args := Cmd_Args + 1;
     if Ada.Command_Line.Argument (Cmd_Args) = "-d" then
        Evaluation.Debug := True;
     elsif Ada.Command_Line.Argument (Cmd_Args) = "-e" then
        Envs.Debug := True;
     elsif not File_Processed then
        File_Param := Cmd_Args;
        File_Processed := True;
     else
        Command_List.Append
          (Types.New_Symbol_Mal_Type (Ada.Command_Line.Argument (Cmd_Args)));
     end if;

   end loop;

   Envs.Set (Envs.Get_Current, "*ARGV*", Command_Args);

   if File_Processed then
      Ada.Text_IO.Put_Line
        (Rep ("(load-file """ & Ada.Command_Line.Argument (File_Param) & """)"));
   end if;

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
end Step8_Macros;
