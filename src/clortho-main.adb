--  with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;

with Clortho.Command_Line;
with Clortho.Logging;
with Clortho.Configuration;
with Clortho.Command_Dispatchers;
with Clortho.Db;
with Clortho.DB_Keys;
with Clortho.Exit_Statuses;

procedure Clortho.Main is

   pragma SPARK_Mode;

   --
   --  Wrapper to make SPARK happy, otherwise is complains that
   --  Set_Exit_Status has no global contract. It is OK since
   --  it is the last procedure called before returning
   --
   procedure Set_Exit_Status (X : Ada.Command_Line.Exit_Status)
     with
       Global => null;

   procedure Set_Exit_Status (X : Ada.Command_Line.Exit_Status)
   is
      pragma SPARK_Mode (Off);
   begin
      Ada.Command_Line.Set_Exit_Status (X);
   end Set_Exit_Status;

begin
   if DB_Keys.Is_Agent_Call then
      DB_Keys.Work_As_Agent;
      return;
   end if;

   declare
      Config : constant Clortho.Command_Line.Parsed_CLI :=
                 Clortho.Command_Line.Parse_Command_Line;
   begin
      if not Command_Line.Is_Ok (Config) then
         Logging.Print_To_Stderr (Command_Line.Error_Message (Config));
         Set_Exit_Status (Ada.Command_Line.Failure);
         return;
      end if;

      declare
         Dispatcher : Command_Dispatchers.Dispatcher_Type :=
                        Command_Dispatchers.New_Dispatcher (Config);

         Status     : Exit_Statuses.Exit_Status;

         DB_Key     : constant DB_Keys.DB_Key_Type := DB_Keys.Get;
      begin
         Db.Use_DB (Filename => Configuration.DB_Filename,
                    DB_Key   => DB_Key,
                    User     => Dispatcher,
                    Success  => Status);

         if not Exit_Statuses.Is_Success (Status) then
            Logging.Print_To_Stderr (Exit_Statuses.Reason (Status));
            Set_Exit_Status (Ada.Command_Line.Failure);

         else
            Set_Exit_Status (Ada.Command_Line.Success);

         end if;
      end;
   end;
end Clortho.Main;
