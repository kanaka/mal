with Ada.Strings.Hash;

package body Types.Strings is

   function "=" (Left  : in Instance;
                 Right : in String) return Boolean
   is (Left.Data = Right);

   function Alloc (Data : in String) return String_Ptr is
      Ref : constant String_Ptr := new Instance (Data'Length);
   begin
      Garbage_Collected.Register (Garbage_Collected.Pointer (Ref));
      Ref.all.Data := Data;
      return Ref;
   end Alloc;

   function Hash (Item : in String_Ptr) return Ada.Containers.Hash_Type
   is (Ada.Strings.Hash (Item.all.Data));

   procedure Query_Element
     (Container : in Instance;
      Process   : not null access procedure (Element : in String))
   is
   begin
      Process.all (Container.Data);
   end Query_Element;

   function Same_Contents (Left, Right : in String_Ptr) return Boolean
   is (Left = Right or else Left.all.Data = Right.all.Data);

   function To_String (Container : in Instance) return String
   is (Container.Data);

end Types.Strings;
