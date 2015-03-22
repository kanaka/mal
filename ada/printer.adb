package body Printer is

   function Pr_Str (M : Types.Smart_Pointer) return String is
      use Types;
   begin
     if M.Pointer = null then
        return "";
     else
        return To_String (Types.Deref(M).all);
     end if;
   end Pr_Str;

end Printer;
