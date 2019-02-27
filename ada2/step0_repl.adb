with Ada.Text_IO;
with Interfaces.C.Strings;

procedure Step0_Repl is

   subtype Mal_Type is String;

   function Read (Source : in String) return Mal_Type
   is (Source);

   function Eval (Ast : in Mal_Type) return Mal_Type
   is (Ast);

   function Print (Ast : in Mal_Type) return String
   is (Ast);

   function Rep (Source : in String) return String
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
            Ada.Text_IO.Put_Line (Rep (Line));
         end;
      end loop;
      Ada.Text_IO.New_Line;
   end Interactive_Loop;

   ----------------------------------------------------------------------

begin
   Interactive_Loop;
end Step0_Repl;
