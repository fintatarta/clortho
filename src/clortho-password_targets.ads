with Clortho.Exit_Statuses;

package Clortho.Password_Targets is
   type Target_Name is
     (
      Standard_Output,
      Clipboard
     );

   procedure Send_Password_To (Password : String;
                               Target   : Target_Name;
                               Status   : out Exit_Statuses.Exit_Status);
end Clortho.Password_Targets;
