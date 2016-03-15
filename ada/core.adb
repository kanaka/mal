with Ada.Characters.Latin_1;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Envs;
with Eval_Callback;
with Reader;
with Smart_Pointers;
with Types;
with Types.Hash_Map;
with Types.Vector;

package body Core is

   use Types;

   -- primitive functions on Smart_Pointer,
   function "+" is new Arith_Op ("+", "+");
   function "-" is new Arith_Op ("-", "-");
   function "*" is new Arith_Op ("*", "*");
   function "/" is new Arith_Op ("/", "/");

   function "<" is new Rel_Op ("<", "<");
   function "<=" is new Rel_Op ("<=", "<=");
   function ">" is new Rel_Op (">", ">");
   function ">=" is new Rel_Op (">=", ">=");


   function Eval_As_Boolean (MH : Types.Mal_Handle) return Boolean is
      use Types;
      Res : Boolean;
   begin
      case Deref (MH).Sym_Type is
         when Bool => 
            Res := Deref_Bool (MH).Get_Bool;
         when Sym =>
            return not (Deref_Sym (MH).Get_Sym = "nil");
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


   function Do_Eval (Rest_Handle : Mal_Handle; Env : Envs.Env_Handle)
   return Types.Mal_Handle is
      First_Param : Mal_Handle;
      Rest_List : Types.List_Mal_Type;
   begin
      Rest_List := Deref_List (Rest_Handle).all;
      First_Param := Car (Rest_List);
      return Eval_Callback.Eval.all (First_Param, Env);
   end Do_Eval;


   function Throw (Rest_Handle : Mal_Handle; Env : Envs.Env_Handle)
   return Types.Mal_Handle is
      First_Param : Mal_Handle;
      Rest_List : Types.List_Mal_Type;
   begin
      Rest_List := Deref_List (Rest_Handle).all;
      First_Param := Car (Rest_List);
      Types.Mal_Exception_Value := First_Param;
      raise Mal_Exception;
      return First_Param;  -- Keep the compiler happy.
   end Throw;


   function Is_True (Rest_Handle : Mal_Handle; Env : Envs.Env_Handle)
   return Types.Mal_Handle is
      First_Param, Evaled_List : Mal_Handle;
      Rest_List : Types.List_Mal_Type;
   begin
      Rest_List := Deref_List (Rest_Handle).all;
      First_Param := Car (Rest_List);
      return New_Bool_Mal_Type
        (Deref (First_Param).Sym_Type = Bool and then
         Deref_Bool (First_Param).Get_Bool);
   end Is_True;


   function Is_False (Rest_Handle : Mal_Handle; Env : Envs.Env_Handle)
   return Types.Mal_Handle is
      First_Param, Evaled_List : Mal_Handle;
      Rest_List : Types.List_Mal_Type;
   begin
      Rest_List := Deref_List (Rest_Handle).all;
      First_Param := Car (Rest_List);
      return New_Bool_Mal_Type
        (Deref (First_Param).Sym_Type = Bool and then
         not Deref_Bool (First_Param).Get_Bool);
   end Is_False;


   function Is_Nil (Rest_Handle : Mal_Handle; Env : Envs.Env_Handle)
   return Types.Mal_Handle is
      First_Param, Evaled_List : Mal_Handle;
      Rest_List : Types.List_Mal_Type;
   begin
      Rest_List := Deref_List (Rest_Handle).all;
      First_Param := Car (Rest_List);
      return New_Bool_Mal_Type
        (Deref (First_Param).Sym_Type = Sym and then
         Deref_Sym (First_Param).Get_Sym = "nil");
   end Is_Nil;


   function Meta (Rest_Handle : Mal_Handle; Env : Envs.Env_Handle)
   return Types.Mal_Handle is
      First_Param : Mal_Handle;
      Rest_List : Types.List_Mal_Type;
   begin
      Rest_List := Deref_List (Rest_Handle).all;
      First_Param := Car (Rest_List);
      return Deref (First_Param).Get_Meta;
   end Meta;


   function With_Meta (Rest_Handle : Mal_Handle; Env : Envs.Env_Handle)
   return Types.Mal_Handle is
      First_Param, Meta_Param, Res : Mal_Handle;
      Rest_List : Types.List_Mal_Type;
   begin
      Rest_List := Deref_List (Rest_Handle).all;
      First_Param := Car (Rest_List);
      Rest_List := Deref_List (Cdr (Rest_List)).all;
      Meta_Param := Car (Rest_List);
      Res := Copy (First_Param);
      Deref (Res).Set_Meta (Meta_Param);
      return Res;
   end With_Meta;


   function New_Atom (Rest_Handle : Mal_Handle; Env : Envs.Env_Handle)
   return Types.Mal_Handle is
      First_Param : Mal_Handle;
      Rest_List : Types.List_Mal_Type;
   begin
      Rest_List := Deref_List (Rest_Handle).all;
      First_Param := Car (Rest_List);
      return New_Atom_Mal_Type (First_Param);
   end New_Atom;

   function Is_Atom (Rest_Handle : Mal_Handle; Env : Envs.Env_Handle)
   return Types.Mal_Handle is
      First_Param, Evaled_List : Mal_Handle;
      Rest_List : Types.List_Mal_Type;
   begin
      Rest_List := Deref_List (Rest_Handle).all;
      First_Param := Car (Rest_List);
      return New_Bool_Mal_Type (Deref (First_Param).Sym_Type = Atom);
   end Is_Atom;


   function Deref (Rest_Handle : Mal_Handle; Env : Envs.Env_Handle)
   return Types.Mal_Handle is
      First_Param : Mal_Handle;
      Rest_List : Types.List_Mal_Type;
   begin
      Rest_List := Deref_List (Rest_Handle).all;
      First_Param := Car (Rest_List);
      return Deref_Atom (First_Param).Get_Atom;
   end Deref;


   function Reset (Rest_Handle : Mal_Handle; Env : Envs.Env_Handle)
   return Types.Mal_Handle is
      First_Param, Atom_Param, New_Val : Mal_Handle;
      Rest_List : Types.List_Mal_Type;
   begin
      Rest_List := Deref_List (Rest_Handle).all;
      Atom_Param := Car (Rest_List);
      Rest_List := Deref_List (Cdr (Rest_List)).all;
      New_Val := Car (Rest_List);
      Deref_Atom (Atom_Param).Set_Atom (New_Val);
      return New_Val;
   end Reset;


   function Swap (Rest_Handle : Mal_Handle; Env : Envs.Env_Handle)
   return Types.Mal_Handle is
      First_Param, Atom_Param, Atom_Val, New_Val : Mal_Handle;
      Rest_List : Types.List_Mal_Type;
      Rest_List_Class : Types.List_Class_Ptr;
      Func_Param, Param_List : Mal_Handle;
   begin
      Rest_List := Deref_List (Rest_Handle).all;
      Atom_Param := Car (Rest_List);
      Rest_List := Deref_List (Cdr (Rest_List)).all;
      Func_Param := Car (Rest_List);
      Param_List := Cdr (Rest_List);

      Rest_List_Class := Deref_List_Class (Param_List);
      Param_List := Rest_List_Class.Duplicate;
      Atom_Val := Deref_Atom (Atom_Param).Get_Atom;
      Param_List := Prepend (Atom_Val, Deref_List (Param_List).all);
      case Deref (Func_Param).Sym_Type is
         when Lambda =>
            New_Val := Deref_Lambda (Func_Param).Apply (Param_List);
         when Func =>
            New_Val := Deref_Func (Func_Param).Call_Func (Param_List, Env);
         when others => raise Mal_Exception with "Swap with bad func";
      end case;
      Deref_Atom (Atom_Param).Set_Atom (New_Val);
      return New_Val;
   end Swap;


   function Is_List (Rest_Handle : Mal_Handle; Env : Envs.Env_Handle)
   return Types.Mal_Handle is
      First_Param, Evaled_List : Mal_Handle;
      Rest_List : Types.List_Mal_Type;
   begin
      Rest_List := Deref_List (Rest_Handle).all;
      First_Param := Car (Rest_List);
      return New_Bool_Mal_Type
        (Deref (First_Param).Sym_Type = List and then
         Deref_List (First_Param).Get_List_Type = List_List);
   end Is_List;


   function Is_Vector (Rest_Handle : Mal_Handle; Env : Envs.Env_Handle)
   return Types.Mal_Handle is
      First_Param, Evaled_List : Mal_Handle;
      Rest_List : Types.List_Mal_Type;
   begin
      Rest_List := Deref_List (Rest_Handle).all;
      First_Param := Car (Rest_List);
      return New_Bool_Mal_Type
        (Deref (First_Param).Sym_Type = List and then
         Deref_List (First_Param).Get_List_Type = Vector_List);
   end Is_Vector;


   function Is_Map (Rest_Handle : Mal_Handle; Env : Envs.Env_Handle)
   return Types.Mal_Handle is
      First_Param, Evaled_List : Mal_Handle;
      Rest_List : Types.List_Mal_Type;
   begin
      Rest_List := Deref_List (Rest_Handle).all;
      First_Param := Car (Rest_List);
      return New_Bool_Mal_Type
        (Deref (First_Param).Sym_Type = List and then
         Deref_List (First_Param).Get_List_Type = Hashed_List);
   end Is_Map;


   function Is_Sequential (Rest_Handle : Mal_Handle; Env : Envs.Env_Handle)
   return Types.Mal_Handle is
      First_Param, Evaled_List : Mal_Handle;
      Rest_List : Types.List_Mal_Type;
   begin
      Rest_List := Deref_List (Rest_Handle).all;
      First_Param := Car (Rest_List);
      return New_Bool_Mal_Type
        (Deref (First_Param).Sym_Type = List and then
         Deref_List (First_Param).Get_List_Type /= Hashed_List);
   end Is_Sequential;


   function Is_Empty (Rest_Handle : Mal_Handle; Env : Envs.Env_Handle)
   return Types.Mal_Handle is
      First_Param, Evaled_List : Mal_Handle;
      List : List_Class_Ptr;
      Rest_List : Types.List_Mal_Type;
   begin
      Rest_List := Deref_List (Rest_Handle).all;
      First_Param := Car (Rest_List);
      List := Deref_List_Class (First_Param);
      return New_Bool_Mal_Type (Is_Null (List.all));
   end Is_Empty;


   function Eval_As_List (MH : Types.Mal_Handle) return List_Mal_Type is
   begin
      case Deref (MH).Sym_Type is
         when List =>  return Deref_List (MH).all;
         when Sym =>
            if Deref_Sym (MH).Get_Sym = "nil" then
               return Null_List (List_List);
            end if;
         when others => null;
      end case;
      raise Evaluation_Error with "Expecting a List";
      return Null_List (List_List);
   end Eval_As_List;


   function Count (Rest_Handle : Mal_Handle; Env : Envs.Env_Handle)
   return Types.Mal_Handle is
      First_Param, Evaled_List : Mal_Handle;
      L : List_Mal_Type;
      Rest_List : Types.List_Mal_Type;
      N : Natural;
   begin
      Rest_List := Deref_List (Rest_Handle).all;
      First_Param := Car (Rest_List);
      if Deref (First_Param).Sym_Type = List and then
         Deref_List (First_Param).Get_List_Type = Vector_List then
         N := Deref_List_Class (First_Param).Length;
      else
         L := Eval_As_List (First_Param);
         N := L.Length;
      end if;
      return New_Int_Mal_Type (N);
   end Count;


   function Cons (Rest_Handle : Mal_Handle; Env : Envs.Env_Handle)
   return Types.Mal_Handle is
      Rest_List : Types.List_Mal_Type;
      First_Param, List_Handle : Mal_Handle;
      List : List_Mal_Type;
      List_Class : List_Class_Ptr;
   begin
      Rest_List := Deref_List (Rest_Handle).all;
      First_Param := Car (Rest_List);
      List_Handle := Cdr (Rest_List);
      List := Deref_List (List_Handle).all;
      List_Handle := Car (List);
      List_Class := Deref_List_Class (List_Handle);
      return Prepend (First_Param, List_Class.all);
   end Cons;


   function Concat (Rest_Handle : Mal_Handle; Env : Envs.Env_Handle)
   return Types.Mal_Handle is
      Rest_List : Types.List_Mal_Type;
   begin
      Rest_List := Deref_List (Rest_Handle).all;
      return Types.Concat (Rest_List, Env);
   end Concat;


   function First (Rest_Handle : Mal_Handle; Env : Envs.Env_Handle)
   return Types.Mal_Handle is
      Rest_List : Types.List_Mal_Type;
      First_List : Types.List_Class_Ptr;
      First_Param : Mal_Handle;
   begin
      Rest_List := Deref_List (Rest_Handle).all;
      First_Param := Car (Rest_List);
      if Deref (First_Param).Sym_Type = Sym then
         return New_Symbol_Mal_Type ("nil");
      end if;
      First_List := Deref_List_Class (First_Param);
      if Is_Null (First_List.all) then
         return New_Symbol_Mal_Type ("nil");
      else
         return Types.Car (First_List.all);
      end if;
   end First;


   function Rest (Rest_Handle : Mal_Handle; Env : Envs.Env_Handle)
   return Types.Mal_Handle is
      Rest_List : Types.List_Mal_Type;
      First_Param, Container : Mal_Handle;
   begin
      Rest_List := Deref_List (Rest_Handle).all;
      First_Param := Car (Rest_List);
      if Deref (First_Param).Sym_Type = Sym then
         -- Assuming it's nil
         return New_List_Mal_Type (List_List);
      end if;
      Container := Deref_List_Class (First_Param).Cdr;
      return Deref_List_Class (Container).Duplicate;
   end Rest;


   function Nth (Rest_Handle : Mal_Handle; Env : Envs.Env_Handle)
   return Types.Mal_Handle is
      -- Rest_List, First_List : Types.List_Mal_Type;
      Rest_List : Types.List_Mal_Type;
      First_List : Types.List_Class_Ptr;
      First_Param, List_Handle, Num_Handle : Mal_Handle;
      List : List_Mal_Type;
      Index : Types.Int_Mal_Type;
   begin
      Rest_List := Deref_List (Rest_Handle).all;
      First_Param := Car (Rest_List);
      First_List := Deref_List_Class (First_Param);
      List_Handle := Cdr (Rest_List);
      List := Deref_List (List_Handle).all;
      Num_Handle := Car (List);
      Index := Deref_Int (Num_Handle).all;
      return Types.Nth (First_List.all, Natural (Index.Get_Int_Val));
   end Nth;


   function Map (Rest_Handle : Mal_Handle; Env : Envs.Env_Handle)
   return Types.Mal_Handle is

      Rest_List, Results_List : List_Mal_Type;
      Func_Handle, List_Handle, Results_Handle : Mal_Handle;

   begin

      -- The rest of the line.
      Rest_List := Deref_List (Rest_Handle).all;

      Func_Handle := Car (Rest_List);
      List_Handle := Nth (Rest_List, 1);

      Results_Handle := New_List_Mal_Type (List_List);
      Results_List := Deref_List (Results_Handle).all;

      while not Is_Null (Deref_List_Class (List_Handle).all) loop

         declare
            Parts_Handle : Mal_Handle;
         begin
            Parts_Handle :=
              Make_New_List
                ((1 => Func_Handle,
                  2 => Make_New_List
                         ((1 => New_Symbol_Mal_Type ("quote"),
                           2 => Car (Deref_List_Class (List_Handle).all)))));
 
            List_Handle := Cdr (Deref_List_Class (List_Handle).all);

            -- Using a Parts_Handle below doesn't work.
            Append
              (Results_List,
               Eval_Callback.Eval.all (Parts_Handle, Env));

         end;

      end loop;

      return New_List_Mal_Type (Results_List);

   end Map;


   function Apply (Rest_Handle : Mal_Handle; Env : Envs.Env_Handle)
   return Types.Mal_Handle is

      Results_Handle : Mal_Handle;
      Rest_List, Results_List : List_Mal_Type;

   begin

      -- The rest of the line.
      Rest_List := Deref_List (Rest_Handle).all;

      Results_Handle := New_List_Mal_Type (List_List);
      Results_List := Deref_List (Results_Handle).all;

      -- Just bundle all the parts into one list so
      -- (apply f (A B) (C D)) becomes (f A B C D)
      -- Is this right? Or should it be (f (A B) C D)?
      while not Is_Null (Rest_List) loop
         declare
            Part_Handle : Mal_Handle;
         begin
            Part_Handle := Car (Rest_List);
            Rest_List := Deref_List (Cdr (Rest_List)).all;

            if Deref (Part_Handle).Sym_Type = List then
               declare
                  The_List : List_Class_Ptr;
                  List_Item : Mal_Handle;
                  Next_List : Mal_Handle;
               begin
                  The_List := Deref_List_Class (Part_Handle);
                  while not Is_Null (The_List.all) loop
                     List_Item := Car (The_List.all);
                     Append (Results_List, List_Item);
                     Next_List := Cdr (The_List.all);
                     The_List := Deref_List_Class (Next_List);
                  end loop;
               end;
            else
               Append (Results_List, Part_Handle);
            end if;
         end;
      end loop;
      return Eval_Callback.Eval.all (New_List_Mal_Type (Results_List), Env);
   end Apply;


   function Symbol (Rest_Handle : Mal_Handle; Env : Envs.Env_Handle)
   return Types.Mal_Handle is

      Sym_Handle, Res : Mal_Handle;
      Rest_List : List_Mal_Type;

   begin

      -- The rest of the line.
      Rest_List := Deref_List (Rest_Handle).all;

      Sym_Handle := Car (Rest_List);

      declare
        The_String : Mal_String :=
          Deref_String (Sym_Handle).Get_String;
      begin

         Res := New_Symbol_Mal_Type
                  (The_String (The_String'First + 1 .. The_String'Last - 1));

      end;
      return Res;
   end Symbol;


   function Is_Symbol (Rest_Handle : Mal_Handle; Env : Envs.Env_Handle)
   return Types.Mal_Handle is

      Sym_Handle : Mal_Handle;
      Rest_List : List_Mal_Type;
      Res : Boolean;

   begin
      Rest_List := Deref_List (Rest_Handle).all;
      Sym_Handle := Car (Rest_List);
      if Deref (Sym_Handle).Sym_Type = Sym then
         Res := Deref_Sym (Sym_Handle).Get_Sym (1) /= ':';
      else
         Res := False;
      end if;
      return New_Bool_Mal_Type (Res);
   end Is_Symbol;


   function Keyword (Rest_Handle : Mal_Handle; Env : Envs.Env_Handle)
   return Types.Mal_Handle is

      Sym_Handle, Res : Mal_Handle;
      Rest_List : List_Mal_Type;

   begin

      -- The rest of the line.
      Rest_List := Deref_List (Rest_Handle).all;

      Sym_Handle := Car (Rest_List);

      declare
        The_String : Mal_String :=
          Deref_String (Sym_Handle).Get_String;
      begin

         Res := New_Symbol_Mal_Type
                  (':' & The_String (The_String'First + 1 .. The_String'Last - 1));

      end;
      return Res;
   end Keyword;


   function Is_Keyword (Rest_Handle : Mal_Handle; Env : Envs.Env_Handle)
   return Types.Mal_Handle is

      Sym_Handle : Mal_Handle;
      Rest_List : List_Mal_Type;
      Res : Boolean;

   begin
      Rest_List := Deref_List (Rest_Handle).all;
      Sym_Handle := Car (Rest_List);
      if Deref (Sym_Handle).Sym_Type = Sym then
         Res := Deref_Sym (Sym_Handle).Get_Sym (1) = ':';
      else
         Res := False;
      end if;
      return New_Bool_Mal_Type (Res);
   end Is_Keyword;


   function New_List (Rest_Handle : Mal_Handle; Env : Envs.Env_Handle)
   return Types.Mal_Handle is
      Rest_List : Types.List_Mal_Type;
   begin
      Rest_List := Deref_List (Rest_Handle).all;
      return New_List_Mal_Type (The_List => Rest_List);
   end New_List;


   function New_Vector (Rest_Handle : Mal_Handle; Env : Envs.Env_Handle)
   return Types.Mal_Handle is
      Rest_List : List_Mal_Type;
      Res : Mal_Handle;
      use Types.Vector;
   begin
      Res := New_Vector_Mal_Type;
      Rest_List := Deref_List (Rest_Handle).all;
      while not Is_Null (Rest_List) loop
         Deref_Vector (Res).Append (Car (Rest_List));
         Rest_List := Deref_List (Cdr (Rest_List)).all;
      end loop;
      return Res;
   end New_Vector;


   function New_Map (Rest_Handle : Mal_Handle; Env : Envs.Env_Handle)
   return Types.Mal_Handle is
      Rest_List : List_Mal_Type;
      Res : Mal_Handle;
   begin
      Res := Hash_Map.New_Hash_Map_Mal_Type;
      Rest_List := Deref_List (Rest_Handle).all;
      while not Is_Null (Rest_List) loop
         Hash_Map.Deref_Hash (Res).Append (Car (Rest_List));
         Rest_List := Deref_List (Cdr (Rest_List)).all;
      end loop;
      return Res;
   end New_Map;


   function Assoc (Rest_Handle : Mal_Handle; Env : Envs.Env_Handle)
   return Types.Mal_Handle is
      Rest_List : Mal_Handle;
      Map : Hash_Map.Hash_Map_Mal_Type;
   begin
      Rest_List := Rest_Handle;
      Map := Hash_Map.Deref_Hash (Car (Deref_List (Rest_List).all)).all;
      Rest_List := Cdr (Deref_List (Rest_List).all);
      return Hash_Map.Assoc (Map, Rest_List);
   end Assoc;


   function Dis_Assoc (Rest_Handle : Mal_Handle; Env : Envs.Env_Handle)
   return Types.Mal_Handle is
      Rest_List : Mal_Handle;
      Map : Hash_Map.Hash_Map_Mal_Type;
   begin
      Rest_List := Rest_Handle;
      Map := Hash_Map.Deref_Hash (Car (Deref_List (Rest_List).all)).all;
      Rest_List := Cdr (Deref_List (Rest_List).all);
      return Hash_Map.Dis_Assoc (Map, Rest_List);
   end Dis_Assoc;


   function Get_Key (Rest_Handle : Mal_Handle; Env : Envs.Env_Handle)
   return Types.Mal_Handle is
      Rest_List : List_Mal_Type;
      Map : Hash_Map.Hash_Map_Mal_Type;
      Map_Param, Key : Mal_Handle;
      The_Sym : Sym_Types;
   begin

      Rest_List := Deref_List (Rest_Handle).all;
      Map_Param := Car (Rest_List);
      The_Sym := Deref (Map_Param).Sym_Type;
      if The_Sym = Sym then
         -- Either its nil or its some other atom
         -- which makes no sense!
         return New_Symbol_Mal_Type ("nil");
      end if;

      -- Assume a map from here on in.
      Map := Hash_Map.Deref_Hash (Car (Rest_List)).all;
      Rest_List := Deref_List (Cdr (Rest_List)).all;
      Key := Car (Rest_List);

      return Map.Get (Key);

   end Get_Key;


   function Contains_Key (Rest_Handle : Mal_Handle; Env : Envs.Env_Handle)
   return Types.Mal_Handle is
      Rest_List : List_Mal_Type;
      Map : Hash_Map.Hash_Map_Mal_Type;
      Key : Mal_Handle;
   begin
      Rest_List := Deref_List (Rest_Handle).all;
      Map := Hash_Map.Deref_Hash (Car (Rest_List)).all;
      Rest_List := Deref_List (Cdr (Rest_List)).all;
      Key := Car (Rest_List);
      return New_Bool_Mal_Type (Hash_Map.Contains (Map, Key));
   end Contains_Key;


   function All_Keys (Rest_Handle : Mal_Handle; Env : Envs.Env_Handle)
   return Types.Mal_Handle is
      Rest_List : List_Mal_Type;
      Map : Hash_Map.Hash_Map_Mal_Type;
   begin
      Rest_List := Deref_List (Rest_Handle).all;
      Map := Hash_Map.Deref_Hash (Car (Rest_List)).all;
      return Hash_Map.All_Keys (Map);
   end All_Keys;


   function All_Values (Rest_Handle : Mal_Handle; Env : Envs.Env_Handle)
   return Types.Mal_Handle is
      Rest_List : List_Mal_Type;
      Map : Hash_Map.Hash_Map_Mal_Type;
   begin
      Rest_List := Deref_List (Rest_Handle).all;
      Map := Hash_Map.Deref_Hash (Car (Rest_List)).all;
      return Hash_Map.All_Values (Map);
   end All_Values;


   -- Take a list with two parameters and produce a single result
   -- using the Op access-to-function parameter.
   function Reduce2
     (Op : Binary_Func_Access; LH : Mal_Handle; Env : Envs.Env_Handle)
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


   function Plus (Rest_Handle : Mal_Handle; Env : Envs.Env_Handle)
   return Types.Mal_Handle is
   begin
       return Reduce2 ("+"'Access, Rest_Handle, Env);
   end Plus;


   function Minus (Rest_Handle : Mal_Handle; Env : Envs.Env_Handle)
   return Types.Mal_Handle is
   begin
       return Reduce2 ("-"'Access, Rest_Handle, Env);
   end Minus;


   function Mult (Rest_Handle : Mal_Handle; Env : Envs.Env_Handle)
   return Types.Mal_Handle is
   begin
       return Reduce2 ("*"'Access, Rest_Handle, Env);
   end Mult;


   function Divide (Rest_Handle : Mal_Handle; Env : Envs.Env_Handle)
   return Types.Mal_Handle is
   begin
       return Reduce2 ("/"'Access, Rest_Handle, Env);
   end Divide;


   function LT (Rest_Handle : Mal_Handle; Env : Envs.Env_Handle)
   return Types.Mal_Handle is
   begin
       return Reduce2 ("<"'Access, Rest_Handle, Env);
   end LT;


   function LTE (Rest_Handle : Mal_Handle; Env : Envs.Env_Handle)
   return Types.Mal_Handle is
   begin
       return Reduce2 ("<="'Access, Rest_Handle, Env);
   end LTE;


   function GT (Rest_Handle : Mal_Handle; Env : Envs.Env_Handle)
   return Types.Mal_Handle is
   begin
       return Reduce2 (">"'Access, Rest_Handle, Env);
   end GT;


   function GTE (Rest_Handle : Mal_Handle; Env : Envs.Env_Handle)
   return Types.Mal_Handle is
   begin
       return Reduce2 (">="'Access, Rest_Handle, Env);
   end GTE;


   function EQ (Rest_Handle : Mal_Handle; Env : Envs.Env_Handle)
   return Types.Mal_Handle is
   begin
       return Reduce2 (Types."="'Access, Rest_Handle, Env);
   end EQ;


   function Pr_Str (Rest_Handle : Mal_Handle; Env : Envs.Env_Handle)
   return Types.Mal_Handle is
      use Ada.Strings.Unbounded;
      Res : Unbounded_String;
   begin
      return New_String_Mal_Type ('"' & Deref_List (Rest_Handle).Pr_Str & '"');
   end Pr_Str;


   function Prn (Rest_Handle : Mal_Handle; Env : Envs.Env_Handle)
   return Types.Mal_Handle is
      use Ada.Strings.Unbounded;
   begin
      Ada.Text_IO.Put_Line (Deref_List (Rest_Handle).Pr_Str);
      return New_Symbol_Mal_Type ("nil");
   end Prn;


   function Println (Rest_Handle : Mal_Handle; Env : Envs.Env_Handle)
   return Types.Mal_Handle is
      use Ada.Strings.Unbounded;
      Res : String := Deref_List (Rest_Handle).Pr_Str (False);
   begin
      Ada.Text_IO.Put_Line (Res);
      return New_Symbol_Mal_Type ("nil");
   end Println;


   function Str (Rest_Handle : Mal_Handle; Env : Envs.Env_Handle)
   return Types.Mal_Handle is
      use Ada.Strings.Unbounded;
      Res : String := Deref_List (Rest_Handle).Cat_Str (False);
   begin
      return New_String_Mal_Type ('"' & Res & '"');
   end Str;


   function Read_String (Rest_Handle : Mal_Handle; Env : Envs.Env_Handle)
   return Types.Mal_Handle is
      Rest_List : Types.List_Mal_Type;
      First_Param : Mal_Handle;
   begin
      Rest_List := Deref_List (Rest_Handle).all;
      First_Param := Car (Rest_List);
      declare
         Str_Param : String := Deref_String (First_Param).Get_String;
         Unquoted_Str : String(1 .. Str_Param'Length-2) :=
           Str_Param (Str_Param'First+1 .. Str_Param'Last-1);
         -- i.e. strip out the double-qoutes surrounding the string.
      begin
         return Reader.Read_Str (Unquoted_Str);
      end;
   end Read_String;


   function Read_Line (Rest_Handle : Mal_Handle; Env : Envs.Env_Handle)
   return Types.Mal_Handle is
      Rest_List : Types.List_Mal_Type;
      First_Param : Mal_Handle;
      S : String (1..Reader.Max_Line_Len);
      Last : Natural;
   begin
      Rest_List := Deref_List (Rest_Handle).all;
      First_Param := Car (Rest_List);
      declare
         Prompt : String := Deref_String (First_Param).Get_String;
      begin
         -- Print the prompt, less the quote marks.
         Ada.Text_IO.Put (Prompt (2 .. Prompt'Last - 1));
      end;
      Ada.Text_IO.Get_Line (S, Last);
      return New_String_Mal_Type ('"' & S (1 .. Last) & '"');
   end Read_Line;


   function Slurp (Rest_Handle : Mal_Handle; Env : Envs.Env_Handle)
   return Types.Mal_Handle is
      Rest_List : Types.List_Mal_Type;
      First_Param : Mal_Handle;
   begin
      Rest_List := Deref_List (Rest_Handle).all;
      First_Param := Car (Rest_List);
      declare
         Str_Param : String := Deref_String (First_Param).Get_String;
         Unquoted_Str : String(1 .. Str_Param'Length-2) :=
           Str_Param (Str_Param'First+1 .. Str_Param'Last-1);
         -- i.e. strip out the double-qoutes surrounding the string.
         use Ada.Text_IO;
         Fn : Ada.Text_IO.File_Type;
         Line_Str : String (1..Reader.Max_Line_Len);
         File_Str : Ada.Strings.Unbounded.Unbounded_String :=
           Ada.Strings.Unbounded.To_Unbounded_String ("""");
         Last : Natural;
         I : Natural := 0;
      begin
         Ada.Text_IO.Open (Fn, In_File, Unquoted_Str);
         while not End_Of_File (Fn) loop
            Get_Line (Fn, Line_Str, Last);
            if Last > 0 then
               Ada.Strings.Unbounded.Append (File_Str, Line_Str (1 .. Last));
               Ada.Strings.Unbounded.Append (File_Str, Ada.Characters.Latin_1.LF);
            end if;
         end loop;
         Ada.Text_IO.Close (Fn);
         Ada.Strings.Unbounded.Append (File_Str, '"');
         return New_String_Mal_Type (Ada.Strings.Unbounded.To_String (File_Str));
      end;
   end Slurp;


   function Conj (Rest_Handle : Mal_Handle; Env : Envs.Env_Handle)
   return Types.Mal_Handle is
      Rest_List : List_Mal_Type;
      First_Param, Res : Mal_Handle;
   begin
      Rest_List := Deref_List (Rest_Handle).all;
      First_Param := Car (Rest_List);
      Rest_List := Deref_List (Cdr (Rest_List)).all;

      -- Is this a List or a Vector?
      case Deref_List (First_Param).Get_List_Type is
         when List_List =>
            Res := Copy (First_Param);
            while not Is_Null (Rest_List) loop
               Res := Prepend (To_List => Deref_List (Res).all, Op => Car (Rest_List));
               Rest_List := Deref_List (Cdr (Rest_List)).all;
            end loop;
            return Res;
         when Vector_List =>
            Res := Copy (First_Param);
            while not Is_Null (Rest_List) loop
               Vector.Append (Vector.Deref_Vector (Res).all, Car (Rest_List));
               Rest_List := Deref_List (Cdr (Rest_List)).all;
            end loop;
            return Res;
         when Hashed_List => raise Mal_Exception with "Conj on Hashed_Map";
      end case;
   end Conj;


   procedure Init is
     use Envs;
   begin

      Envs.New_Env;

      Set (Get_Current, "*host-language*", Types.New_Symbol_Mal_Type ("ada"));

      Set (Get_Current, "true", Types.New_Bool_Mal_Type (True));

      Set (Get_Current,
           "true?",
           New_Func_Mal_Type ("true?", Is_True'access));

      Set (Get_Current, "false", Types.New_Bool_Mal_Type (False));

      Set (Get_Current,
           "false?",
           New_Func_Mal_Type ("false?", Is_False'access));

      Set (Get_Current, "nil", Types.New_Symbol_Mal_Type ("nil"));

      Set (Get_Current,
           "meta",
           New_Func_Mal_Type ("meta", Meta'access));

      Set (Get_Current,
           "with-meta",
           New_Func_Mal_Type ("with-meta", With_Meta'access));

      Set (Get_Current,
           "nil?",
           New_Func_Mal_Type ("nil?", Is_Nil'access));

      Set (Get_Current,
           "throw",
           New_Func_Mal_Type ("throw", Throw'access));

      Set (Get_Current,
           "atom",
           New_Func_Mal_Type ("atom", New_Atom'access));

      Set (Get_Current,
           "atom?",
           New_Func_Mal_Type ("atom?", Is_Atom'access));

      Set (Get_Current,
           "deref",
           New_Func_Mal_Type ("deref", Core.Deref'access));

      Set (Get_Current,
           "reset!",
           New_Func_Mal_Type ("reset!", Reset'access));

      Set (Get_Current,
           "swap!",
           New_Func_Mal_Type ("swap!", Swap'access));

      Set (Get_Current,
           "list",
           New_Func_Mal_Type ("list", New_List'access));

      Set (Get_Current,
           "list?",
           New_Func_Mal_Type ("list?", Is_List'access));

      Set (Get_Current,
           "vector",
           New_Func_Mal_Type ("vector", New_Vector'access));

      Set (Get_Current,
           "vector?",
           New_Func_Mal_Type ("vector?", Is_Vector'access));

      Set (Get_Current,
           "hash-map",
           New_Func_Mal_Type ("hash-map", New_Map'access));

      Set (Get_Current,
           "assoc",
           New_Func_Mal_Type ("assoc", Assoc'access));

      Set (Get_Current,
           "dissoc",
           New_Func_Mal_Type ("dissoc", Dis_Assoc'access));

      Set (Get_Current,
           "get",
           New_Func_Mal_Type ("get", Get_Key'access));

      Set (Get_Current,
           "keys",
           New_Func_Mal_Type ("keys", All_Keys'access));

      Set (Get_Current,
           "vals",
           New_Func_Mal_Type ("vals", All_Values'access));

      Set (Get_Current,
           "map?",
           New_Func_Mal_Type ("map?", Is_Map'access));

      Set (Get_Current,
           "contains?",
           New_Func_Mal_Type ("contains?", Contains_Key'access));

      Set (Get_Current,
           "sequential?",
           New_Func_Mal_Type ("sequential?", Is_Sequential'access));

      Set (Get_Current,
           "empty?",
           New_Func_Mal_Type ("empty?", Is_Empty'access));

      Set (Get_Current,
           "count",
           New_Func_Mal_Type ("count", Count'access));

      Set (Get_Current,
           "cons",
           New_Func_Mal_Type ("cons", Cons'access));

      Set (Get_Current,
           "concat",
           New_Func_Mal_Type ("concat", Concat'access));

      Set (Get_Current,
           "first",
           New_Func_Mal_Type ("first", First'access));

      Set (Get_Current,
           "rest",
           New_Func_Mal_Type ("rest", Rest'access));

      Set (Get_Current,
           "nth",
           New_Func_Mal_Type ("nth", Nth'access));

      Set (Get_Current,
           "map",
           New_Func_Mal_Type ("map", Map'access));

      Set (Get_Current,
           "apply",
           New_Func_Mal_Type ("apply", Apply'access));

      Set (Get_Current,
           "symbol",
           New_Func_Mal_Type ("symbol", Symbol'access));

      Set (Get_Current,
           "symbol?",
           New_Func_Mal_Type ("symbol?", Is_Symbol'access));

      Set (Get_Current,
           "keyword",
           New_Func_Mal_Type ("keyword", Keyword'access));

      Set (Get_Current,
           "keyword?",
           New_Func_Mal_Type ("keyword?", Is_Keyword'access));

      Set (Get_Current,
           "pr-str",
           New_Func_Mal_Type ("pr-str", Pr_Str'access));

      Set (Get_Current,
           "str",
           New_Func_Mal_Type ("str", Str'access));

      Set (Get_Current,
           "prn",
           New_Func_Mal_Type ("prn", Prn'access));

      Set (Get_Current,
           "println",
           New_Func_Mal_Type ("println", Println'access));

      Set (Get_Current,
           "eval",
           New_Func_Mal_Type ("eval", Do_Eval'access));

      Set (Get_Current,
           "read-string",
           New_Func_Mal_Type ("read-string", Read_String'access));

      Set (Get_Current,
           "readline",
           New_Func_Mal_Type ("readline", Read_Line'access));

      Set (Get_Current,
           "slurp",
           New_Func_Mal_Type ("slurp", Slurp'access));

      Set (Get_Current,
           "conj",
           New_Func_Mal_Type ("conj", Conj'access));

      Set (Get_Current,
           "+",
           New_Func_Mal_Type ("+", Plus'access));

      Set (Get_Current,
           "-",
           New_Func_Mal_Type ("-", Minus'access));

      Set (Get_Current,
           "*",
           New_Func_Mal_Type ("*", Mult'access));

      Set (Get_Current,
           "/",
           New_Func_Mal_Type ("/", Divide'access));

      Set (Get_Current,
           "<",
           New_Func_Mal_Type ("<", LT'access));

      Set (Get_Current,
           "<=",
           New_Func_Mal_Type ("<=", LTE'access));

      Set (Get_Current,
           ">",
           New_Func_Mal_Type (">", GT'access));

      Set (Get_Current,
           ">=",
           New_Func_Mal_Type (">=", GTE'access));

      Set (Get_Current,
           "=",
           New_Func_Mal_Type ("=", EQ'access));

   end Init;


end Core;
