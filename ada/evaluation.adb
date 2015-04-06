with Ada.Text_IO;
with Envs;
with Smart_Pointers;

package body Evaluation is


   -- primitive functions on Smart_Pointer,
   function "+" is new Types.Op ("+", "+");
   function "-" is new Types.Op ("-", "-");
   function "*" is new Types.Op ("*", "*");
   function "/" is new Types.Op ("/", "/");


   procedure Add_Defs (Defs : Types.List_Mal_Type) is
      use Types;
      Nil : Types.List_Mal_Type := Null_List (Defs.Get_List_Type);
      D, L : List_Mal_Type;
   begin
      if Debug then
         Ada.Text_IO.Put_Line ("Add_Defs " & To_String (Defs));
      end if;
      D := Defs;
      while D /= Nil loop
         L := Deref_List (Cdr (D)).all;
         Envs.Set
           (Deref_Atom (Car (D)).Get_Atom,
            Eval (Car (L)));
         D := Deref_List (Cdr(L)).all;
      end loop;
   end Add_Defs;

   function Apply (Func : Types.Mal_Handle; Args : Types.List_Mal_Type)
   return Types.Mal_Handle is
      use Types;
   begin

      if Debug then

         Ada.Text_IO.Put_Line
           ("Applying " & To_String (Deref (Func).all) &
            " to " & Args.To_String);

      end if;

      case Deref (Func).Sym_Type is

         when Atom =>

            declare
               Atom_P : Types.Atom_Ptr;
            begin
               Atom_P := Types.Deref_Atom (Func);
               if Atom_P.Get_Atom = "+" then
                  return Reduce ("+"'Access, Args);
               elsif Atom_P.Get_Atom = "-" then
                  return Reduce ("-"'Access, Args);
               elsif Atom_P.Get_Atom = "*" then
                  return Reduce ("*"'Access, Args);
               elsif Atom_P.Get_Atom = "/" then
                  return Reduce ("/"'Access, Args);
               end if;
           end;

         when Error => return Func;

         when others => null;

      end case;
      return Smart_Pointers.Null_Smart_Pointer;
   end Apply;


   function Def_Fn (Args : Types.List_Mal_Type) return Types.Mal_Handle is
      use Types;
      Name, Fn_Body, Res : Mal_Handle;
   begin
      Name := Car (Args);
      pragma Assert (Deref (Name).Sym_Type = Atom,
                     "Def_Fn: expected atom as name");
      Fn_Body := Car (Deref_List (Cdr (Args)).all);
      Res := Eval (Fn_Body);
      Envs.Set (Deref_Atom (Name).Get_Atom, Res);
      return Res;
   end Def_Fn;


   function Let_Processing (Args : Types.List_Mal_Type)
   return Types.Mal_Handle is
      use Types;
      Defs, Expr, Res : Mal_Handle;
   begin
      Envs.New_Env;
      Defs := Car (Args);
      Add_Defs (Deref_List (Defs).all);
      Expr := Car (Deref_List (Cdr (Args)).all);
      Res := Eval (Expr);
      Envs.Delete_Env;
      return Res;
   end Let_Processing;


   function Eval_Ast
     (Ast : Types.Mal_Handle)
   return Types.Mal_Handle is
      use Types;
   begin

      case Deref (Ast).Sym_Type is

         when Atom =>

            declare
              Sym : Mal_String := Deref_Atom (Ast).Get_Atom;
            begin
               -- if keyword...
               if Sym(1) = ':' then
                  return Ast;
               else
                  return Envs.Get (Sym);
               end if;
            exception
               when Envs.Not_Found =>
                  return New_Error_Mal_Type ("'" &  Sym & "' not found");
            end;

         when List =>

            return Map (Eval'Access, Deref_List (Ast).all);

         when others => return Ast;

      end case;

   end Eval_Ast;


   function List_Processing (L : Types.Mal_Handle)
   return Types.Mal_Handle is
      use Types;
      pragma Assert (Deref (L).Sym_Type = List,
                     "List_Processing: expected a list");
      Evaled_List : List_Mal_Type;
      Func : Mal_Handle;
      Args : List_Mal_Type;
   begin
      Evaled_List := Deref_List (Eval_Ast (L)).all;
      Func := Car (Evaled_List);
      Args := Deref_List (Cdr (Evaled_List)).all;
      return Apply (Func, Args);
   end List_Processing;


   function Eval_List (L : Types.Mal_Handle) return Types.Mal_Handle is
      use Types;
      pragma Assert (Deref (L).Sym_Type = List,
                     "Eval_List: expected a List");
      LMT : List_Mal_Type;
      First_Elem : Mal_Handle;
   begin

      LMT := Deref_List (L).all;

      First_Elem := Car (LMT);

      if Deref (First_Elem).Sym_Type = Atom then

         declare
            Atom_P : Atom_Ptr;
         begin
            Atom_P := Deref_Atom (First_Elem);
            if Atom_P.Get_Atom = "def!" then
               return Def_Fn (Deref_List (Cdr (LMT)).all);
            elsif Atom_P.Get_Atom = "let*" then
               return Let_Processing (Deref_List (Cdr (LMT)).all);
            else -- not a special form
               return List_Processing (L);
            end if;
         end;

      else -- First elem in list is not an atom
         return List_Processing (L);
      end if;

   end Eval_List;


   function Eval (Param : Types.Mal_Handle)
   return Types.Mal_Handle is
      use Types;
      First_Elem : Mal_Handle;
   begin

      if Debug then
         Ada.Text_IO.Put_Line ("Evaling " & Deref (Param).To_String);
      end if;

      if Deref (Param).Sym_Type = List and then
         Deref_List (Param).all.Get_List_Type = List_List then

         return Eval_List (Param);

      else

         return Eval_Ast (Param);

      end if;

   end Eval;


end Evaluation;
