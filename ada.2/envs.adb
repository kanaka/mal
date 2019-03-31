with Ada.Text_IO.Unbounded_IO;

with Err;
with Printer;
with Types.Sequences;
with Types.Symbols.Names;

package body Envs is

   use Types;

   ----------------------------------------------------------------------

   procedure Dump_Stack (Env : in Instance) is
      use Ada.Text_IO;
   begin
      Put_Line ("environment:");
      for P in Env.Data.Iterate loop
         --  Do not print builtins for repl.
         if HM.Element (P).Kind /= Kind_Builtin or Env.Outer /= null then
            Put ("   ");
            Put (HM.Key (P).To_String);
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
                 Key : in Symbols.Ptr) return Mal.T
   is
      --  Trust the compiler to detect the tail call. A loop would
      --  require a Ptr parameter or a separated first iteration.
      Position : constant HM.Cursor := Env.Data.Find (Key);
   begin
      if HM.Has_Element (Position) then
         return HM.Element (Position);
      end if;
      Err.Check (Env.Outer /= null,
                 "'" & Symbols.To_String (Key) & "' not found");
      return Env.Outer.all.Get (Key);
   end Get;

   procedure Keep_References (Object : in out Instance) is
      --  Same remarks than for Get.
   begin
      for Element of Object.Data loop
         Mal.Keep (Element);
      end loop;
      if Object.Outer /= null then
         Object.Outer.all.Keep;
      end if;
   end Keep_References;

   function New_Env (Outer : in Ptr                  := null;
                     Binds : in Symbols.Symbol_Array := No_Binds;
                     Exprs : in Mal.T_Array          := No_Exprs) return Ptr
   is
      use type Symbols.Ptr;
      Ref : constant Ptr := new Instance'(Garbage_Collected.Instance with
                                          Outer => Outer,
                                          Data  => HM.Empty_Map);
   begin
      Garbage_Collected.Register (Garbage_Collected.Pointer (Ref));
      if 2 <= Binds'Length
        and then Binds (Binds'Last - 1) = Symbols.Names.Ampersand
      then
         Err.Check (Binds'Length - 2 <= Exprs'Length,
                    "not enough actual parameters for vararg function");
         for I in 0 .. Binds'Length - 3 loop
            Ref.all.Data.Include (Key      => Binds (Binds'First + I),
                                  New_Item => Exprs (Exprs'First + I));
         end loop;
         Ref.all.Data.Include (Key      => Binds (Binds'Last),
                               New_Item => Sequences.List
                       (Exprs (Exprs'First + Binds'Length - 2 .. Exprs'Last)));
      else
         Err.Check (Binds'Length = Exprs'Length,
                    "wrong parameter count for (not vararg) function");
         for I in 0 .. Binds'Length - 1 loop
            Ref.all.Data.Include (Key      => Binds (Binds'First + I),
                                  New_Item => Exprs (Exprs'First + I));
         end loop;
      end if;
      return Ref;
   end New_Env;

   procedure Set (Env      : in out Instance;
                  Key      : in     Symbols.Ptr;
                  New_Item : in     Mal.T)
   is
   begin
      Env.Data.Include (Key, New_Item);
   end Set;

end Envs;
