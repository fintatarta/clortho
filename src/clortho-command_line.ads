with Clortho.Option_Sets;
with Clortho.Commands;
with Clortho.Flagged_Types;
with Clortho.Password_Targets;
with Clortho.Exit_Statuses;

with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;

package Clortho.Command_Line is
   pragma SPARK_Mode;

   use Clortho.Commands;

   type Parsed_CLI is private;

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

   function Requested_Version (Item : Parsed_CLI) return Positive
     with
       Pre =>
         Is_Ok (Item)
         and then Command (Item) = Get_Password;

   function User_Provided_Password (Item : Parsed_CLI) return Boolean
     with
       Pre =>
         Is_Ok (Item)
         and then Command (Item) in Creation_Command;

   function User_Password (Item : Parsed_CLI) return String
     with
       Pre =>
         Is_Ok (Item)
         and then Command (Item) in Creation_Command
         and then User_Provided_Password (Item);

   function Password_Spec (Item : Parsed_CLI)
                           return Flagged_Types.Flagged_String
     with
       Pre =>
         Is_Ok (Item)
         and then Command (Item) in Creation_Command
         and then not User_Provided_Password (Item);

   function Password_Length (Item : Parsed_CLI) return Option_Sets.Char_Length
     with
       Pre =>
         Is_Ok (Item)
         and then Command (Item) in Creation_Command
         and then not User_Provided_Password (Item);

   function Password_Nbits (Item : Parsed_CLI) return Option_Sets.Entropy;

   function Target (Item : Parsed_CLI) return Password_Targets.Target_Name
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
      Double_Specs,
      Missing_Key,
      Unexpected_Key,
      Bad_Command_Line
     );

   subtype Error_With_Explanation is
     Error_Status range Unknown_Option .. Bad_Integer;

   subtype Error_Without_Explanation is
     Error_Status range Double_Action .. Bad_Command_Line;

   subtype Option_Processing_Error is
     Error_Status range Ok .. Double_Specs;

   subtype Key_Processing_Error is
     Error_Status range Missing_Key .. Bad_Command_Line;

   type Parsed_CLI  is
      record
         Status : Exit_Statuses.Exit_Status;

         Options : Option_Sets.Option_Set;
         Key     : Flagged_Types.Flagged_String;
      end record;

   --  Wrap arounds library functions to make SPARK happy
   function Argument_Count return Natural
     with
       Global => null;

   function Argument (N : Positive) return String
     with
       Global => null,
       Pre => N <= Argument_Count;

   procedure Get_Line (Item : out Unbounded_String)
     with
       Global => null;

end Clortho.Command_Line;
