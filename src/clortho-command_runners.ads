with Clortho.Commands;          use Clortho.Commands;
with Clortho.Command_Line;
with Clortho.Exit_Statuses;     use Clortho.Exit_Statuses;
with Clortho.Db;

package Clortho.Command_Runners is

   procedure Get_Password (Config         : Command_Line.Parsed_CLI;
                           Db             : in out Clortho.Db.Password_Db;
                           Command_Status : out Exit_Status)
     with
       Pre =>
         Command_Line.Is_Ok (Config)
         and then Command_Line.Command (Config) = Get_Password;

   procedure Create_Entry (Config         : Command_Line.Parsed_CLI;
                           Db             : in out Clortho.Db.Password_Db;
                           Command_Status : out Exit_Status)
     with
       Pre =>
         Command_Line.Is_Ok (Config)
         and then Command_Line.Command (Config) = Create_Entry;

   procedure Renew_Password (Config         : Command_Line.Parsed_CLI;
                             Db             : in out Clortho.Db.Password_Db;
                             Command_Status : out Exit_Status)
     with
       Pre =>
         Command_Line.Is_Ok (Config)
         and then Command_Line.Command (Config) = Renew_Password;

   procedure Vacuum_Entry (Config         : Command_Line.Parsed_CLI;
                           Db             : in out Clortho.Db.Password_Db;
                           Command_Status : out Exit_Status)
     with
       Pre =>
         Command_Line.Is_Ok (Config)
         and then Command_Line.Command (Config) = Vacuum_Entry;

   procedure Roll_Back_Entry (Config         : Command_Line.Parsed_CLI;
                              Db             : in out Clortho.Db.Password_Db;
                              Command_Status : out Exit_Status)
     with
       Pre =>
         Command_Line.Is_Ok (Config)
         and then Command_Line.Command (Config) = Roll_Back_Entry;

   procedure Delete_Entry (Config         : Command_Line.Parsed_CLI;
                           Db             : in out Clortho.Db.Password_Db;
                           Command_Status : out Exit_Status)
     with
       Pre =>
         Command_Line.Is_Ok (Config)
         and then Command_Line.Command (Config) = Delete_Entry;

   procedure Vacuum_All (Config         : Command_Line.Parsed_CLI;
                         Db             : in out Clortho.Db.Password_Db;
                         Command_Status : out Exit_Status)
     with
       Pre =>
         Command_Line.Is_Ok (Config)
         and then Command_Line.Command (Config) = Vacuum_All;

   procedure List (Config         : Command_Line.Parsed_CLI;
                   Db             : in out Clortho.Db.Password_Db;
                   Command_Status : out Exit_Status)
     with
       Pre =>
         Command_Line.Is_Ok (Config)
         and then Command_Line.Command (Config) = List;
end Clortho.Command_Runners;
