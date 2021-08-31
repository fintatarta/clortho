with Clortho.Password_Conditions;
package Clortho.Command_Line is
   pragma SPARK_Mode;

   type Command_Type is
     (
      Get_Password,
      Create_Entry,
      Renew_Password,
      Vacuum_Entry,
      Roll_Back_Entry,
      Delete_Entry,
      Vacuum_All
     );

   subtype Command_With_Parameter
     is Command_Type range Get_Password .. Delete_Entry;

   subtype Creation_Command
     is Command_Type range Create_Entry .. Renew_Password;

   subtype Password_Writing_Command
     is Command_Type range Get_Password .. Renew_Password;

   type Target_Name is
     (
      Standard_Output,
      Clipboard
     );


   type Parsed_CLI (<>) is private;

   function Parse_Command_Line return Parsed_CLI;

   function Is_Ok (Item : Parsed_CLI) return Boolean;

   function Error_Message (Item : Parsed_CLI) return String
     with
       Pre => not Is_Ok (Item);

   function Command (Item : Parsed_CLI) return Command_Type
     with
       Pre => Is_Ok (Item);

   function Entry_Key (Item : Parsed_CLI) return String
     with
       Pre =>
         Is_Ok (Item)
         and then (Command (Item) in Command_With_Parameter);

   function Use_Provided_Password (Item : Parsed_CLI) return Boolean
     with
       Pre =>
         Is_Ok (Item)
         and then Command (Item) in Creation_Command;

   function User_Password (Item : Parsed_CLI) return String
     with
       Pre =>
         Is_Ok (Item)
         and then Command (Item) in Creation_Command
         and then Use_Provided_Password (Item);

   function Password_Spec (Item : Parsed_CLI)
                           return Password_Conditions.Condition_Type
     with
       Pre =>
         Is_Ok (Item)
         and then Command (Item) in Creation_Command
         and then not Use_Provided_Password (Item);

   function Password_Length (Item : Parsed_CLI) return Positive
     with
       Pre =>
         Is_Ok (Item)
         and then Command (Item) in Creation_Command
         and then not Use_Provided_Password (Item);

   function Password_Target (Item : Parsed_CLI) return Target_Name
     with
       Pre =>
         Is_Ok (Item)
         and then Command (Item) in Password_Writing_Command;

private
   type Parsed_CLI is null record;
end Clortho.Command_Line;
