pragma Ada_2012;
package body Clortho.Clipboard is

   -------------------
   -- Get_Clipboard --
   -------------------

   procedure Get_Clipboard (Item : out Ada.Strings.Unbounded.Unbounded_String)
   is
   begin
      Item := Ada.Strings.Unbounded.Null_Unbounded_String;
   end Get_Clipboard;

end Clortho.Clipboard;
