pragma Ada_2012;
package body Clortho.Clipboard is

   -------------------
   -- Get_Clipboard --
   -------------------

   function Get_Clipboard return String is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Get_Clipboard unimplemented");
      return raise Program_Error with "Unimplemented function Get_Clipboard";
   end Get_Clipboard;

end Clortho.Clipboard;
