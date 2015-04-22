with Ada.Text_IO;
with Envs;
with Smart_Pointers;

package body Evaluation is

   use Types;

   -- primitive functions on Smart_Pointer,
   function "+" is new Op ("+", "+");
   function "-" is new Op ("-", "-");
   function "*" is new Op ("*", "*");
   function "/" is new Op ("/", "/");

   function "<" is new Rel_Op ("<", "<");
   function "<=" is new Rel_Op ("<=", "<=");
   function ">" is new Rel_Op (">", ">");
   function ">=" is new Rel_Op (">=", ">=");


   procedure Add_Defs (Defs : List_Mal_Type; Env : Envs.Env_Handle) is
      D, L : List_Mal_Type;
   begin
      if Debug then
         Ada.Text_IO.Put_Line ("Add_Defs " & To_String (Defs));
      end if;
      D := Defs;
      while not Is_Null (D) loop
         L := Deref_List (Cdr (D)).all;
         Envs.Set
           (Env,
            Deref_Atom (Car (D)).Get_Atom,
            Eval (Car (L), Env));
         D := Deref_List (Cdr(L)).all;
      end loop;
   end Add_Defs;


   function Fn_Processing
     (L : Lambda_Ptr;
      Fn_List : Mal_Handle;
      Env : Envs.Env_Handle)
   return Mal_Handle is

      Params : List_Mal_Type;
      E : Envs.Env_Handle;
   begin
      E := Envs.New_Env (L.Get_Env);
      Params := Deref_List (L.Get_Params).all;
      Envs.Bind (E, Params, Deref_List (Fn_List).all);

      return Eval (L.Get_Expr, E); 

   end Fn_Processing;


   function Apply (Func : Types.Mal_Handle; Params : Types.Mal_Handle)
   return Types.Mal_Handle is
      use Types;
      Args : List_Mal_Type;
   begin

      Args := Deref_List (Params).all;

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
               elsif Atom_P.Get_Atom = "<" then
                  return Reduce ("<"'Access, Args);
               elsif Atom_P.Get_Atom = "<=" then
                  return Reduce ("<="'Access, Args);
               elsif Atom_P.Get_Atom = ">" then
                  return Reduce (">"'Access, Args);
               elsif Atom_P.Get_Atom = ">=" then
                  return Reduce (">="'Access, Args);
               elsif Atom_P.Get_Atom = "=" then
                  return Reduce (Types."="'Access, Args);
               elsif Atom_P.Get_Atom = "list" then
                  return New_List_Mal_Type (The_List => Args);
               end if;
           end;

         when Lambda =>

            declare
               Lam : Lambda_Ptr;
            begin
               Lam := Deref_Lambda (Func);
               return Fn_Processing (Lam, Params, Lam.Get_Env);
            end;

         when Error => return Func;

         when others => null;

      end case;
      return Smart_Pointers.Null_Smart_Pointer;
   end Apply;


   function Def_Fn (Args : Types.List_Mal_Type; Env : Envs.Env_Handle) return Types.Mal_Handle is
      use Types;
      Name, Fn_Body, Res : Mal_Handle;
   begin
      Name := Car (Args);
      pragma Assert (Deref (Name).Sym_Type = Atom,
                     "Def_Fn: expected atom as name");
      Fn_Body := Car (Deref_List (Cdr (Args)).all);
      Res := Eval (Fn_Body, Env);
      Envs.Set (Envs.Get_Current, Deref_Atom (Name).Get_Atom, Res);
      return Res;
   end Def_Fn;


   function Let_Processing (Args : Types.List_Mal_Type; Env : Envs.Env_Handle)
   return Types.Mal_Handle is
      use Types;
      Defs, Expr, Res : Mal_Handle;
   begin
      Envs.New_Env;
      Defs := Car (Args);
      Add_Defs (Deref_List (Defs).all, Envs.Get_Current);
      Expr := Car (Deref_List (Cdr (Args)).all);
      Res := Eval (Expr, Envs.Get_Current);
      Envs.Delete_Env;
      return Res;
   end Let_Processing;


   function Eval_As_Boolean (MH : Types.Mal_Handle) return Boolean is
      use Types;
      Res : Boolean;
   begin
      case Deref (MH).Sym_Type is
         when Bool => 
            Res := Deref_Bool (MH).Get_Bool;
         when Atom =>
            return not (Deref_Atom (MH).Get_Atom = "nil");
--         when List =>
--            declare
--               L : List_Mal_Type;
--            begin
--               L := Deref_List (MH).all;
--               Res := not Is_Null (L);
--            end;
         when others => -- Everything else
            Res := True;
      end case;
      return Res;
   end Eval_As_Boolean;


   function If_Processing (Args : Types.List_Mal_Type; Env : Envs.Env_Handle)
   return Types.Mal_Handle is

      Cond, True_Part, False_Part : Mal_Handle;
      Cond_Bool : Boolean;
      pragma Assert (Length (Args) = 2 or Length (Args) = 3,
                     "If_Processing: not 2 or 3 parameters");
      L : List_Mal_Type;
   begin
      
      Cond := Eval (Car (Args), Env);

      Cond_Bool := Eval_As_Boolean (Cond);

      if Cond_Bool then
         L := Deref_List (Cdr (Args)).all;
         return Eval (Car (L), Env);
      else
         if Length (Args) = 3 then
            L := Deref_List (Cdr (Args)).all;
            L := Deref_List (Cdr (L)).all;
            return Eval (Car (L), Env);
         else
            return New_Atom_Mal_Type ("nil");
         end if;
      end if;
   end If_Processing;


   function Eval_Ast
     (Ast : Types.Mal_Handle; Env : Envs.Env_Handle)
   return Types.Mal_Handle is

      function Call_Eval (A : Mal_Handle) return Mal_Handle is
      begin
         return Eval (A, Env);
      end Call_Eval;

   begin

      case Deref (Ast).Sym_Type is

         when Atom =>

            declare
               Sym : Mal_String := Deref_Atom (Ast).Get_Atom;
            begin
               -- if keyword or nil (which may represent False)...
               if Sym(1) = ':' then
                  return Ast;
               else
                  return Envs.Get (Env, Sym);
               end if;
            exception
               when Envs.Not_Found =>
                  return New_Error_Mal_Type ("'" &  Sym & "' not found");
            end;

         when List =>

            return Map (Call_Eval'Unrestricted_Access, Deref_List (Ast).all);

         when Lambda =>

            -- Evaluating a lambda in a different Env.
            declare
               L : Lambda_Ptr;
               New_Env : Envs.Env_Handle;
            begin
               L := Deref_Lambda (Ast);
               New_Env := Env;
               -- Make the current Lambda's env the outer of the env param.
               Envs.Set_Outer (New_Env, L.Get_Env);
               -- Make the Lambda's Env.
               L.Set_Env (New_Env);
               return Ast;
            end;

         when others => return Ast;

      end case;

   end Eval_Ast;


   function Do_Processing (Do_List : Types.List_Mal_Type; Env : Envs.Env_Handle)
   return Types.Mal_Handle is
      use Types;
      D : List_Mal_Type;
      Res : Mal_Handle := Smart_Pointers.Null_Smart_Pointer;
   begin
      if Debug then
         Ada.Text_IO.Put_Line ("Do-ing " & To_String (Do_List));
      end if;
      D := Do_List;
      while not Is_Null (D) loop
         Res := Eval (Car (D), Env);
         D := Deref_List (Cdr(D)).all;
      end loop;
      return Res;
   end Do_Processing;


   function Eval_As_List (MH : Types.Mal_Handle) return List_Mal_Type is
   begin
      case Deref (MH).Sym_Type is
         when List =>  return Deref_List (MH).all;
         when Atom =>
            if Deref_Atom (MH).Get_Atom = "nil" then
               return Null_List (List_List);
            end if;
         when others => null;
      end case;
      raise Evaluation_Error with "Expecting a List";
      return Null_List (List_List);
   end Eval_As_List;


   function Apply2
     (Op : Binary_Func_Access; L : List_Mal_Type; Env : Envs.Env_Handle)
   return Mal_Handle is
      Left, Right : Mal_Handle;
      Rest_List : List_Mal_Type;
   begin
      Left := Eval (Car (L), Env);
      Rest_List := Deref_List (Cdr (L)).all;
      Right := Eval (Car (Rest_List), Env);
      return Op (Left, Right);
   end Apply2;


   function Eval_List (L : Mal_Handle; Env : Envs.Env_Handle)
   return Mal_Handle is

      pragma Assert (Deref (L).Sym_Type = List,
                     "Eval_List: expected a List");
      LMT, Rest_List : List_Mal_Type;
      First_Elem, Rest_Handle : Mal_Handle;

   begin

      LMT := Deref_List (L).all;

      First_Elem := Car (LMT);

      Rest_List := Deref_List (Cdr (LMT)).all;

      case Deref (First_Elem).Sym_Type is

         when Int | Floating | Bool | Str =>
 
            return Eval_Ast (L, Env);

         when Atom =>

            declare
               Atom_P : Atom_Ptr;
            begin
               Atom_P := Deref_Atom (First_Elem);
               if Atom_P.Get_Atom = "def!" then
                  return Def_Fn (Rest_List, Env);
               elsif Atom_P.Get_Atom = "let*" then
                  return Let_Processing (Rest_List, Env);
               elsif Atom_P.Get_Atom = "do" then
                  return Do_Processing (Rest_List, Env);
               elsif Atom_P.Get_Atom = "if" then
                  return If_Processing (Rest_List, Env);
               elsif Atom_P.Get_Atom = "list?" then
                  declare
                     First_Param, Evaled_List : Mal_Handle;
                  begin
                     First_Param := Car (Rest_List);
                     Evaled_List := Eval (First_Param, Env);
                     return New_Bool_Mal_Type
                       (Deref (Evaled_List).Sym_Type = List and then
                        Deref_List (Evaled_List).Get_List_Type = List_List);
                  end;
               elsif Atom_P.Get_Atom = "empty?" then
                  declare
                     First_Param, Evaled_List : Mal_Handle;
                     List : List_Mal_Type;
                  begin
                     First_Param := Car (Rest_List);
                     Evaled_List := Eval (First_Param, Env);
                     List := Deref_List (Evaled_List).all;
                     return New_Bool_Mal_Type (Is_Null (List));
                  end;
               elsif Atom_P.Get_Atom = "count" then
                  declare
                     First_Param, Evaled_List : Mal_Handle;
                     List : List_Mal_Type;
                  begin
                     First_Param := Car (Rest_List);
                     Evaled_List := Eval (First_Param, Env);
                     List := Eval_As_List (Evaled_List);
                     return New_Int_Mal_Type (Length (List));
                  end;
               elsif Atom_P.Get_Atom = "+" then
                  return Apply2 ("+"'Access, Rest_List, Env);
               elsif Atom_P.Get_Atom = "-" then
                  return Apply2 ("-"'Access, Rest_List, Env);
               elsif Atom_P.Get_Atom = "*" then
                  return Apply2 ("*"'Access, Rest_List, Env);
               elsif Atom_P.Get_Atom = "/" then
                  return Apply2 ("/"'Access, Rest_List, Env);
               elsif Atom_P.Get_Atom = "<" then
                  return Apply2 ("<"'Access, Rest_List, Env);
               elsif Atom_P.Get_Atom = "<=" then
                  return Apply2 ("<="'Access, Rest_List, Env);
               elsif Atom_P.Get_Atom = ">" then
                  return Apply2 (">"'Access, Rest_List, Env);
               elsif Atom_P.Get_Atom = ">=" then
                  return Apply2 (">="'Access, Rest_List, Env);
               elsif Atom_P.Get_Atom = "=" then
                  return Apply2 (Types."="'Access, Rest_List, Env);
               elsif Atom_P.Get_Atom = "list" then
                  return New_List_Mal_Type (The_List => Rest_List);
               else -- not a special form

                  declare
                     Res : Mal_Handle;
                  begin
                     -- Eval the atom.
                     Res := Eval_Ast (L, Env);
                     return Eval (Res, Env);
                  end;
               end if;
            end;

         when Lambda =>

            return Fn_Processing
                     (Deref_Lambda (First_Elem),
                      Cdr (LMT),
                      Env);

         when List => 

            -- First elem in the list is a list.
            -- Eval it and then insert it as first elem in the list and
            -- go again.
            declare
               Evaled_List : Mal_Handle;
               E : Envs.Env_Handle;
            begin
               Evaled_List := Eval (First_Elem, Env);
               if Deref (Evaled_List).Sym_Type = Lambda then
                  E := Deref_Lambda (Evaled_List).Get_Env;
               else
                  E := Env;
               end if;
               Evaled_List := Prepend (Evaled_List, Rest_List);
               return Eval (Evaled_List, E);
            end;

         when Error => return First_Elem;

         when Node => return New_Error_Mal_Type ("Evaluating a node");

         when Unitary => null; -- Not yet impl

      end case;

   end Eval_List;


   function Eval (Param : Types.Mal_Handle; Env : Envs.Env_Handle)
   return Types.Mal_Handle is
      use Types;
      First_Elem : Mal_Handle;
   begin

      if Debug then
         Ada.Text_IO.Put_Line ("Evaling " & Deref (Param).To_String);
      end if;

      if Deref (Param).Sym_Type = List and then
         Deref_List (Param).all.Get_List_Type = List_List then

         return Eval_List (Param, Env);

      else

         return Eval_Ast (Param, Env);

      end if;

   end Eval;


end Evaluation;
