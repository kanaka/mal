with Ada.Text_IO;
with Envs;
with Smart_Pointers;

package body Evaluation is


   -- primitive functions on Smart_Pointer,
   function "+" is new Types.Op ("+", "+");
   function "-" is new Types.Op ("-", "-");
   function "*" is new Types.Op ("*", "*");
   function "/" is new Types.Op ("/", "/");


   function Apply (Func : Types.Mal_Handle; Args : Types.List_Mal_Type)
   return Types.Mal_Handle is
      use Types;
   begin
--Ada.Text_IO.Put_Line ("Applying " & To_String (Deref (Func).all) & " to " & Args.To_String);
      case Deref (Func).Sym_Type is
         when Sym =>
            declare
               Sym_P : Types.Sym_Ptr;
            begin
               Sym_P := Types.Deref_Sym (Func);
               case Sym_P.all.Symbol is
                  when '+' => return Reduce ("+"'Access, Args);
                  when '-' => return Reduce ("-"'Access, Args);
                  when '*' => return Reduce ("*"'Access, Args);
                  when '/' => return Reduce ("/"'Access, Args);
                  when others => null;
               end case;
           end;
         when Error => return Func;
         when others => null;
      end case;
      return Smart_Pointers.Null_Smart_Pointer;
   end Apply;


   function Eval_Ast
     (Ast : Types.Mal_Handle)
   return Types.Mal_Handle is
      use Types;
   begin
      case Deref (Ast).Sym_Type is
         when Sym =>
            declare
              Sym : Mal_String (1..1) := Deref_Sym (Ast).Symbol & "";
            begin
               return Envs.Get (Sym);
            exception
               when Envs.Not_Found =>
                  return New_Error_Mal_Type ("'" &  Sym & "' not found");
            end;
         when Atom =>
            declare
              Sym : Mal_String := Deref_Atom (Ast).Get_Atom;
            begin
               return Envs.Get (Sym);
            exception
               when Envs.Not_Found =>
                  return New_Error_Mal_Type ("'" &  Sym & "' not found");
            end;
         when List =>
            return Map (Eval'Access, Deref_List (Ast).all);
         when others =>
            return Ast;
      end case;
   end Eval_Ast;

   function Eval (Param : Types.Mal_Handle)
   return Types.Mal_Handle is
      use Types;
   begin
--Ada.Text_IO.Put_Line ("Evaling " & Deref (Param).To_String);
      if Deref (Param).Sym_Type = List and then
         Deref_List (Param).all.Get_List_Type = List_List then
         declare
            Evaled_List : Types.List_Mal_Type;
            Func : Types.Mal_Handle;
            Args : Types.List_Mal_Type;
         begin
            Evaled_List := Deref_List (Eval_Ast (Param)).all;
            Func := Types.Car (Evaled_List);
            Args := Types.Cdr (Evaled_List);
            return Apply (Func, Args);
         end;
      else
         return Eval_Ast (Param);
      end if;
   end Eval;


end Evaluation;
