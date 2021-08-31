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

   type Password_Target is
     (
      Standard_Output,
      Clipboard
     );

   subtype Command_With_Parameter
     is Command_Type range Get_Password .. Roll_Back_Entry;

   type Parsed_CLI (<>) is private;

   function Parse_Command_Line return Parsed_CLI;

   function Is_Ok (Item : Parsed_CLI) return Boolean;

   function Command (Item : Parsed_CLI) return Command_Type
     with
       Pre => Is_Ok (Item);

   function Entry_Key (Item : Parsed_CLI) return String
     with
       Pre => Is_Ok (Item) and then (Command (Item) in Command_With_Parameter);

   function Use_Provided_Password (Item : Parsed_CLI) return Boolean
     with
       Pre => Is_Ok (Item);

   function User_Password (Item : Parsed_CLI) return String
     with
       Pre => Is_Ok (Item) and then Use_Provided_Password (Item);

   function Password_Spec (Item : Parsed_CLI)
                           return Password_Conditions.Condition_Type
     with
       Pre => Is_Ok (Item) and then not Use_Provided_Password (Item);

   function Password_Length (Item : Parsed_CLI) return Positive
     with
       Pre => Is_Ok (Item) and then not Use_Provided_Password (Item);

private
   type Parsed_CLI is null record;
end Clortho.Command_Line;
