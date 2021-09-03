with Clortho.Commands;          use Clortho.Commands;
with Clortho.Command_Line;

package Clortho.Command_Runners is
   type Command_Exit_Status is private;

   function Is_Ok (Status : Command_Exit_Status) return Boolean;

   function Error_Message (Status : Command_Exit_Status) return String
     with
       Pre => not Is_Ok (Status);

   procedure Get_Password (Config         : Command_Line.Parsed_CLI;
                           Command_Status : out Command_Exit_Status)
     with
       Pre =>
         Command_Line.Is_Ok (Config)
         and then Command_Line.Command (Config) = Get_Password;

   procedure Create_Entry (Config         : Command_Line.Parsed_CLI;
                           Command_Status : out Command_Exit_Status)
     with
       Pre =>
         Command_Line.Is_Ok (Config)
         and then Command_Line.Command (Config) = Create_Entry;

   procedure Renew_Password (Config         : Command_Line.Parsed_CLI;
                             Command_Status : out Command_Exit_Status)
     with
       Pre =>
         Command_Line.Is_Ok (Config)
         and then Command_Line.Command (Config) = Renew_Password;

   procedure Vacuum_Entry (Config         : Command_Line.Parsed_CLI;
                           Command_Status : out Command_Exit_Status)
     with
       Pre =>
         Command_Line.Is_Ok (Config)
         and then Command_Line.Command (Config) = Vacuum_Entry;

   procedure Roll_Back_Entry (Config         : Command_Line.Parsed_CLI;
                              Command_Status : out Command_Exit_Status)
     with
       Pre =>
         Command_Line.Is_Ok (Config)
         and then Command_Line.Command (Config) = Roll_Back_Entry;

   procedure Delete_Entry (Config         : Command_Line.Parsed_CLI;
                           Command_Status : out Command_Exit_Status)
     with
       Pre =>
         Command_Line.Is_Ok (Config)
         and then Command_Line.Command (Config) = Delete_Entry;

   procedure Vacuum_All (Config         : Command_Line.Parsed_CLI;
                         Command_Status : out Command_Exit_Status)
     with
       Pre =>
         Command_Line.Is_Ok (Config)
         and then Command_Line.Command (Config) = Vacuum_All;

   procedure List (Config         : Command_Line.Parsed_CLI;
                   Command_Status : out Command_Exit_Status)
     with
       Pre =>
         Command_Line.Is_Ok (Config)
         and then Command_Line.Command (Config) = List;
private
   type Error_Type is
     (
      Ok,
      Unexpected_Password_Option,
      Generic_Error
     );

   type Command_Exit_Status is
      record
         Err : Error_Type;
      end record;
end Clortho.Command_Runners;
