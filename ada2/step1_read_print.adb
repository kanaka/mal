with Ada.Exceptions;
with Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO;
with Interfaces.C.Strings;

with Printer;
with Reader;
with Types.Mal;

procedure Step1_Read_Print is

   package ASU renames Ada.Strings.Unbounded;
   use Types;

   function Read (Source : in String) return Mal.T
     renames Reader.Read_Str;

   function Eval (Ast : in Mal.T) return Mal.T
   is (Ast);

   function Print (Ast      : in Mal.T;
                   Readably : in Boolean := True) return ASU.Unbounded_String
     renames Printer.Pr_Str;

   function Rep (Source : in String) return ASU.Unbounded_String
   is (Print (Eval (Read (Source)))) with Inline;

   procedure Interactive_Loop;

   ----------------------------------------------------------------------

   procedure Interactive_Loop is
      use Interfaces.C, Interfaces.C.Strings;
      function Readline (Prompt : in char_array) return chars_ptr
        with Import, Convention => C, External_Name => "readline";
      procedure Add_History (Line : in chars_ptr)
        with Import, Convention => C, External_Name => "add_history";
      procedure Free (Line : in chars_ptr)
        with Import, Convention => C, External_Name => "free";
      Prompt : constant char_array := To_C ("user> ");
      C_Line : chars_ptr;
   begin
      loop
         C_Line := Readline (Prompt);
         exit when C_Line = Null_Ptr;
         declare
            Line : constant String := Value (C_Line);
         begin
            if Line /= "" then
               Add_History (C_Line);
            end if;
            Free (C_Line);
            Ada.Text_IO.Unbounded_IO.Put_Line (Rep (Line));
         exception
            when Reader.Empty_Source =>
               null;
            when E : Reader.Reader_Error =>
               Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
            --  Other exceptions are unexpected.
         end;
      end loop;
      Ada.Text_IO.New_Line;
   end Interactive_Loop;

   ----------------------------------------------------------------------

begin
   Interactive_Loop;
end Step1_Read_Print;
