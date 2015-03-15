package body Printer is

   function Pr_Str (M : Types.Mal_Type_Access) return String is
      use Types;
   begin
     if M = null then
        return "";
     else
        return To_String (M.all);
     end if;
   end Pr_Str;

end Printer;
