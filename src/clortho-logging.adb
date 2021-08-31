pragma Ada_2012;
with Ada.Text_IO;

package body Clortho.Logging is

   -----------
   -- Print --
   -----------

   procedure Print_To_Stderr (Msg : String) is
   begin
      Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, Msg);
   end Print_To_Stderr;

end Clortho.Logging;
