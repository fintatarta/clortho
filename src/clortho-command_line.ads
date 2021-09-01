with Clortho.Password_Conditions;
with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;

package Clortho.Command_Line is
   pragma SPARK_Mode;

   type Command_Type is
     (
      Get_Password,
      Get_Old_Password,
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

   type Error_Status is
     (
      Ok,
      Unknown_Option,
      Missing_Parameter,
      Unrequested_Parameter,
      Bad_Option_Syntax,
      Bad_Integer,
      Double_Action,
      Double_Password,
      Double_Password_Length,
      Double_Specs
     );

   subtype Error_With_Explanation is
     Error_Status range Unknown_Option .. Bad_Integer;

   subtype Error_Without_Explanation is
     Error_Status range Double_Action .. Double_Specs;

   type Parsed_CLI (Status          : Error_Status;
                    Name_Length     : Natural)  is
      record
         case Status is
            when Ok =>
               Name            : String (1 .. Name_Length);
               User_Password   : Unbounded_String;
               Command         : Command_Type;
               Target          : Target_Name;
               Specs           : Password_Conditions.Condition_Type;
               Password_Length : Positive;

            when Error_With_Explanation  =>
               Explanation   : Unbounded_String;

            when Error_Without_Explanation =>
               null;
         end case;
      end record;
end Clortho.Command_Line;
