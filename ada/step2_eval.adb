with Ada.Containers.Hashed_Maps;
with Ada.Strings.Unbounded.Hash;
with Ada.Text_IO;
with Ada.IO_Exceptions;
with Ada.Exceptions;
with Printer;
with Reader;
with Smart_Pointers;
with Types;

procedure Step2_Eval is

   use Types;

   -- primitive functions on Smart_Pointer,
   function "+" is new Arith_Op ("+", "+");
   function "-" is new Arith_Op ("-", "-");
   function "*" is new Arith_Op ("*", "*");
   function "/" is new Arith_Op ("/", "/");

   -- Take a list with two parameters and produce a single result
   -- using the Op access-to-function parameter.
   function Reduce2
     (Op : Binary_Func_Access; LH : Mal_Handle)
   return Mal_Handle is
      Left, Right : Mal_Handle;
      L, Rest_List : List_Mal_Type;
   begin
      L := Deref_List (LH).all;
      Left := Car (L);
      Rest_List := Deref_List (Cdr (L)).all;
      Right := Car (Rest_List);
      return Op (Left, Right);
   end Reduce2;


   function Plus (Rest_Handle : Mal_Handle)
   return Types.Mal_Handle is
   begin
       return Reduce2 (Step2_Eval."+"'Unrestricted_Access, Rest_Handle);
   end Plus;


   function Minus (Rest_Handle : Mal_Handle)
   return Types.Mal_Handle is
   begin
       return Reduce2 (Step2_Eval."-"'Unrestricted_Access, Rest_Handle);
   end Minus;


   function Mult (Rest_Handle : Mal_Handle)
   return Types.Mal_Handle is
   begin
       return Reduce2 (Step2_Eval."*"'Unrestricted_Access, Rest_Handle);
   end Mult;


   function Divide (Rest_Handle : Mal_Handle)
   return Types.Mal_Handle is
   begin
       return Reduce2 (Step2_Eval."/"'Unrestricted_Access, Rest_Handle);
   end Divide;


   package String_Mal_Hash is new Ada.Containers.Hashed_Maps
     (Key_Type        => Ada.Strings.Unbounded.Unbounded_String,
      Element_Type    => Smart_Pointers.Smart_Pointer,
      Hash            => Ada.Strings.Unbounded.Hash,
      Equivalent_Keys => Ada.Strings.Unbounded."=",
      "="             => Smart_Pointers."=");

   Not_Found : exception;

   function Get (M : String_Mal_Hash.Map; K : String) return Mal_Handle is
      use String_Mal_Hash;
      C : Cursor;
   begin
      C := Find (M, Ada.Strings.Unbounded.To_Unbounded_String (K));
      if C = No_Element then
         raise Not_Found;
      else
         return Element (C);
      end if;
   end Get;


   Repl_Env : String_Mal_Hash.Map;


   function Eval (Param : Types.Mal_Handle; Env : String_Mal_Hash.Map)
   return Types.Mal_Handle;


   Debug : Boolean := False;


   function Read (Param : String) return Types.Mal_Handle is
   begin
      return Reader.Read_Str (Param);
   end Read;


   function Eval_Ast
     (Ast : Mal_Handle; Env : String_Mal_Hash.Map)
     return Mal_Handle is

      function Call_Eval (A : Mal_Handle) return Mal_Handle is
      begin
         return Eval (A, Env);
      end Call_Eval;

   begin

      case Deref (Ast).Sym_Type is

         when Sym =>

            declare
               Sym : Mal_String := Deref_Sym (Ast).Get_Sym;
            begin
               -- if keyword, return it. Otherwise look it up in the environment.
               if Sym(1) = ':' then
                  return Ast;
               else
                  return Get (Env, Sym);
               end if;
            exception
               when Not_Found =>
                  raise Not_Found with ("'" &  Sym & "' not found");
            end;

         when List =>

            return Map (Call_Eval'Unrestricted_Access, Deref_List_Class (Ast).all);

         when others => return Ast;

      end case;

   end Eval_Ast;


   function Eval (Param : Mal_Handle; Env : String_Mal_Hash.Map)
		 return Mal_Handle is
      First_Elem : Mal_Handle;
   begin

      if Debug then
         Ada.Text_IO.Put_Line ("Evaling " & Deref (Param).To_String);
      end if;

      if Deref (Param).Sym_Type = List and then
 	 Deref_List (Param).Get_List_Type = List_List then

         declare
            Evaled_H, First_Param : Mal_Handle;
            Evaled_List : List_Mal_Type;
         begin
            Evaled_H := Eval_Ast (Param, Env);
            Evaled_List := Deref_List (Evaled_H).all;
            First_Param := Car (Evaled_List);
            return Call_Func (Deref_Func (First_Param).all, Cdr (Evaled_List));
         end;

      else -- Not a List_List

         return Eval_Ast (Param, Env);

      end if;

   end Eval;


   function Print (Param : Types.Mal_Handle) return String is
   begin
      return Printer.Pr_Str (Param);
   end Print;


   function Rep (Param : String; Env : String_Mal_Hash.Map) return String is
     AST, Evaluated_AST : Types.Mal_Handle;
   begin

     AST := Read (Param);

     if Types.Is_Null (AST) then
        return "";
     else
        Evaluated_AST := Eval (AST, Env);
        return Print (Evaluated_AST);
     end if;

   end Rep; 


   S : String (1..Reader.Max_Line_Len);
   Last : Natural;

begin

   String_Mal_Hash.Include
     (Container => Repl_Env,
      Key       => Ada.Strings.Unbounded.To_Unbounded_String ("+"),
      New_Item  => New_Func_Mal_Type ("+", Plus'Unrestricted_access));

   String_Mal_Hash.Include
     (Container => Repl_Env,
      Key       => Ada.Strings.Unbounded.To_Unbounded_String ("-"),
      New_Item  => New_Func_Mal_Type ("-", Minus'Unrestricted_access));

   String_Mal_Hash.Include
     (Container => Repl_Env,
      Key       => Ada.Strings.Unbounded.To_Unbounded_String ("*"),
      New_Item  => New_Func_Mal_Type ("*", Mult'Unrestricted_access));

   String_Mal_Hash.Include
     (Container => Repl_Env,
      Key       => Ada.Strings.Unbounded.To_Unbounded_String ("/"),
      New_Item  => New_Func_Mal_Type ("/", Divide'Unrestricted_access));

   loop
      begin
         Ada.Text_IO.Put ("user> ");
         Ada.Text_IO.Get_Line (S, Last);
         Ada.Text_IO.Put_Line (Rep (S (1..Last), Repl_Env));
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
end Step2_Eval;
