with Interfaces.C.Strings;

package body Readline is

   function Input (Prompt : in String) return String is

      use Interfaces.C;
      use Interfaces.C.Strings;

      function C_Readline (Prompt : in char_array) return chars_ptr
        with Import, Convention => C, External_Name => "readline";

      procedure Add_History (Line : in chars_ptr)
        with Import, Convention => C, External_Name => "add_history";

      procedure Free (Line : in chars_ptr)
        with Import, Convention => C, External_Name => "free";

      C_Line : constant chars_ptr := C_Readline (To_C (Prompt));
   begin
      if C_Line = Null_Ptr then
         raise End_Of_File;
      end if;
      return Ada_Line : constant String := Value (C_Line) do
         if Ada_Line /= "" then
            Add_History (C_Line);
         end if;
         Free (C_Line);
      end return;
   end Input;

end Readline;
