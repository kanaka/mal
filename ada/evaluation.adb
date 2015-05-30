with Ada.Text_IO;
with Envs;
with Smart_Pointers;

package body Evaluation is

   use Types;

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


   function Def_Fn (Args : List_Mal_Type; Env : Envs.Env_Handle)
   return Mal_Handle is
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


   function Let_Processing (Args : List_Mal_Type; Env : Envs.Env_Handle)
   return Mal_Handle is
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


   function Eval_As_Boolean (MH : Mal_Handle) return Boolean is
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


   function Eval_Ast
     (Ast : Mal_Handle; Env : Envs.Env_Handle)
   return Mal_Handle is

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


   function Do_Processing (Do_List : List_Mal_Type; Env : Envs.Env_Handle)
   return Mal_Handle is
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


   function Eval (AParam : Mal_Handle; AnEnv : Envs.Env_Handle)
   return Mal_Handle is
      Param : Mal_Handle;
      Env : Envs.Env_Handle;
      First_Elem : Mal_Handle;
   begin

      Param := AParam;
      Env := AnEnv;

      <<Tail_Call_Opt>>

      if Debug then
         Ada.Text_IO.Put_Line ("Evaling " & Deref (Param).To_String);
      end if;

      if Deref (Param).Sym_Type = List and then
         Deref_List (Param).all.Get_List_Type = List_List then

   declare
      L : Mal_Handle := Param;
      LMT, Rest_List : List_Mal_Type;
      First_Elem, Rest_Handle : Mal_Handle;
   begin

      LMT := Deref_List (L).all;

      First_Elem := Car (LMT);

      Rest_Handle := Cdr (LMT);

      Rest_List := Deref_List (Rest_Handle).all;

      case Deref (First_Elem).Sym_Type is

         when Int | Floating | Bool | Str =>
 
            return First_Elem;

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
                  declare
                     Args : List_Mal_Type := Rest_List;

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

                        Param := Car (L);
                        goto Tail_Call_Opt;
                        -- return Eval (Car (L), Env);
                     else
                        if Length (Args) = 3 then
                           L := Deref_List (Cdr (Args)).all;
                           L := Deref_List (Cdr (L)).all;

                           Param := Car (L);
                           goto Tail_Call_Opt;
                           -- return Eval (Car (L), Env);
                        else
                           return New_Atom_Mal_Type ("nil");
                        end if;
                     end if;
                  end;

               else -- not a special form

                  -- Apply section
                  declare
                     Res : Mal_Handle;
                  begin
                     -- Eval the atom.
                     Res := Eval_Ast (L, Env);
                     Param := Res;
                     goto Tail_Call_Opt;
                     -- return Eval (Res, Env);
                  end;

               end if;
            end;

         when Func =>

            return Call_Func
                     (Deref_Func (First_Elem).all,
                      Rest_Handle,
                      Env);

         when Lambda =>

            declare
               LP : Lambda_Ptr := Deref_Lambda (First_Elem);
               Fn_List : Mal_Handle := Cdr (LMT);
               Params : List_Mal_Type;
               E : Envs.Env_Handle;
            begin
               E := Envs.New_Env (LP.Get_Env);
               Params := Deref_List (LP.Get_Params).all;
               Envs.Bind (E, Params, Deref_List (Fn_List).all);

               Param := LP.Get_Expr;
               Env := E;
               goto Tail_Call_Opt;
               --return Eval (LP.Get_Expr, E); 

            end;

         when List => 

            -- First elem in the list is a list.
            -- Eval it and then insert it as first elem in the list and
            -- go again.
            declare
               Evaled_List : Mal_Handle;
               E : Envs.Env_Handle;
            begin
               Evaled_List := Eval (First_Elem, Env);
               if Is_Null (Evaled_List) then
                  return Evaled_List;
               elsif Deref (Evaled_List).Sym_Type = Lambda then
                  E := Deref_Lambda (Evaled_List).Get_Env;
               else
                  E := Env;
               end if;

               Param := Prepend (Evaled_List, Rest_List);
               Env := E;
               goto Tail_Call_Opt;
               -- Evaled_List := Prepend (Evaled_List, Rest_List);
               -- return Eval (Evaled_List, E);
            end;

         when Error => return First_Elem;

         when Node => return New_Error_Mal_Type ("Evaluating a node");

         when Unitary => null; -- Not yet impl

      end case;

   end;

      else

         return Eval_Ast (Param, Env);

      end if;

   end Eval;


end Evaluation;
