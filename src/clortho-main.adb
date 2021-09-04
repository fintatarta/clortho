--  with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;

--  with Clortho.Password_Generation;
--  with Clortho.Password_Conditions;
--  with Clortho.Password_Style;
with Clortho.Command_Line;
with Clortho.Logging;
with Clortho.Commands;
with Clortho.Command_Dispatchers;
with Clortho.Db;
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
   begin
      Db.Use_DB (Filename => "db.dat",
                 DB_Key   => "",
                 User     => Dispatcher,
                 Success  => status);

      if not Exit_Statuses.Is_Ok (Exit_Status) then
         Logging.Print_To_Stderr (Exit_Statuses.Error_Message (Exit_Status));
         Set_Exit_Status (Ada.Command_Line.Failure);
      else
         Set_Exit_Status (Ada.Command_Line.Success);
      end if;
   end;

end Clortho.Main;
