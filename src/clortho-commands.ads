
package Clortho.Commands is
   type Command_Type is
     (
      Get_Password,
      Create_Entry,
      Renew_Password,
      Vacuum_Entry,
      Roll_Back_Entry,
      Delete_Entry,
      List,
      Vacuum_All
     );

   subtype Command_With_Parameter
     is Command_Type range Get_Password .. Delete_Entry;

   subtype Creation_Command
     is Command_Type range Create_Entry .. Renew_Password;

   subtype Password_Writing_Command
     is Command_Type range Get_Password .. Renew_Password;


   type Source_Name is
     (
      Standard_Input,
      Clipboard
     );

end Clortho.Commands;
