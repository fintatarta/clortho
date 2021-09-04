pragma Ada_2012;
with Clortho.Command_Runners;
with Clortho.Commands;

package body Clortho.Command_Dispatchers is
   pragma SPARK_Mode;
   --------------------
   -- New_Dispatcher --
   --------------------

   function New_Dispatcher
     (Config : Command_Line.Parsed_CLI) return Dispatcher_Type
   is (Dispatcher_Type'(Config => Config));

   --------------
   -- Callback --
   --------------

   overriding procedure Callback
     (User   : in out Dispatcher_Type;
      Db     : in out Clortho.Db.Password_Db;
      Status :    out Exit_Statuses.Exit_Status)
   is
      use Exit_Statuses;
   begin
      case Command_Line.Command (User.Config) is
         when Commands.Get_Password =>
            Command_Runners.Get_Password (User.Config, Db, Status);

         when Commands.Create_Entry =>
            Command_Runners.Create_Entry (User.Config, Db, Status);

         when Commands.Renew_Password =>
            Command_Runners.Renew_Password (User.Config, Db, Status);

         when Commands.Vacuum_Entry =>
            Command_Runners.Vacuum_Entry (User.Config, Db, Status);

         when Commands.Roll_Back_Entry =>
            Command_Runners.Roll_Back_Entry (User.Config, Db, Status);

         when Commands.Delete_Entry =>
            Command_Runners.Delete_Entry (User.Config, Db, Status);

         when Commands.Vacuum_All =>
            Command_Runners.Vacuum_All (User.Config, Db, Status);

         when Commands.List =>
            Command_Runners.List (User.Config, Db, Status);
      end case;

   end Callback;

end Clortho.Command_Dispatchers;
