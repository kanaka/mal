with Ada.Text_IO.Unbounded_IO;

with Err;
with Printer;
with Types.Sequences;

package body Envs is

   use all type Types.Kind_Type;
   use type Types.Strings.Instance;

   ----------------------------------------------------------------------

   procedure Dump_Stack (Env : in Instance) is
      use Ada.Text_IO;
   begin
      Put ("environment:");
      for P in Env.Data.Iterate loop
         --  Do not print builtins for repl.
         if HM.Element (P).Kind /= Kind_Builtin or Env.Outer /= null then
            Put ("   ");
            HM.Key (P).all.Query_Element (Put'Access);
            Put (':');
            Unbounded_IO.Put (Printer.Pr_Str (HM.Element (P)));
            New_Line;
         end if;
      end loop;
      if Env.Outer /= null then
         Put ("outer is ");
         Env.Outer.all.Dump_Stack;
      end if;
   end Dump_Stack;

   function Get (Env : in Instance;
                 Key : in Types.String_Ptr) return Types.T
   is
      Position : HM.Cursor := Env.Data.Find (Key);
      Ref      : Link;
   begin
      if not HM.Has_Element (Position) then
         Ref := Env.Outer;
         loop
            if Ref = null then
               --  Not using Err.Check, which would compute the
               --  argument even if the assertion holds...
               Err.Raise_With ("'" & Key.To_String & "' not found");
            end if;
            Position := Ref.all.Data.Find (Key);
            exit when HM.Has_Element (Position);
            Ref := Ref.all.Outer;
         end loop;
      end if;
      return HM.Element (Position);
   end Get;

   procedure Keep_References (Object : in out Instance) is
   begin
      for Position in Object.Data.Iterate loop
         HM.Key (Position).all.Keep;
         Types.Keep (HM.Element (Position));
      end loop;
      if Object.Outer /= null then
         Object.Outer.all.Keep;
      end if;
   end Keep_References;

   function New_Env (Outer : in Link := null) return Ptr is
      Ref : constant Ptr := new Instance;
   begin
      Garbage_Collected.Register (Garbage_Collected.Pointer (Ref));
      Ref.all.Outer := Outer;
      return Ref;
   end New_Env;

   procedure Set_Binds (Env   : in out Instance;
                        Binds : in     Types.T_Array;
                        Exprs : in     Types.T_Array)
   is
   begin
      if 2 <= Binds'Length and then Binds (Binds'Last - 1).Str.all = "&" then
         Err.Check (Binds'Length - 2 <= Exprs'Length,
                    "not enough actual parameters for vararg function");
         for I in 0 .. Binds'Length - 3 loop
            Env.Data.Include (Key      => Binds (Binds'First + I).Str,
                              New_Item => Exprs (Exprs'First + I));
         end loop;
         Env.Data.Include (Key      => Binds (Binds'Last).Str,
                           New_Item => Types.Sequences.List
                       (Exprs (Exprs'First + Binds'Length - 2 .. Exprs'Last)));
      else
         Err.Check (Binds'Length = Exprs'Length,
                    "wrong parameter count for (not vararg) function");
         for I in 0 .. Binds'Length - 1 loop
            Env.Data.Include (Key      => Binds (Binds'First + I).Str,
                              New_Item => Exprs (Exprs'First + I));
         end loop;
      end if;
   end Set_Binds;

   procedure Set (Env      : in out Instance;
                  Key      : in     Types.T;
                  New_Item : in     Types.T)
   is
   begin
      Err.Check (Key.Kind = Kind_Symbol, "environment keys must be symbols");
      Env.Data.Include (Key.Str, New_Item);
   end Set;

end Envs;
