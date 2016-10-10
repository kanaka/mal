with Ada.Exceptions;
with Ada.Text_IO;
with Interfaces.C.Strings; use type Interfaces.C.Strings.chars_ptr;

procedure Step0_Repl is

   function Read (Source : in String) return String
     is (Source);

   function Eval (Ast : in String) return String
     is (Ast);

   function Print (Ast : in String) return String
     is (Ast);

   function Rep (Source : in String) return String
     is (Print (Eval (Read (Source))));

   procedure Interactive_Loop;

   ----------------------------------------------------------------------

   procedure Interactive_Loop
   is

      function Readline (Prompt : in Interfaces.C.char_array)
                        return Interfaces.C.Strings.chars_ptr
        with Import, Convention => C, External_Name => "readline";

      procedure Add_History (Line : in Interfaces.C.Strings.chars_ptr)
        with Import, Convention => C, External_Name => "add_history";

      procedure Free (Line : in Interfaces.C.Strings.chars_ptr)
        with Import, Convention => C, External_Name => "free";

      Prompt : constant Interfaces.C.char_array
        := Interfaces.C.To_C ("user> ");
      C_Line : Interfaces.C.Strings.chars_ptr;
   begin
      loop
         C_Line := Readline (Prompt);
         exit when C_Line = Interfaces.C.Strings.Null_Ptr;
         declare
            Line : constant String := Interfaces.C.Strings.Value (C_Line);
         begin
            if Line /= "" then
               Add_History (C_Line);
            end if;
            Free (C_Line);
            Ada.Text_IO.Put_Line (Rep (Line));
         exception
            when E : others =>
               Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
               --  but go on proceeding.
         end;
      end loop;
      Ada.Text_IO.New_Line;
   end Interactive_Loop;

   ----------------------------------------------------------------------

begin
   Interactive_Loop;
end Step0_Repl;
