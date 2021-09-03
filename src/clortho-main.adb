--  with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;

--  with Clortho.Password_Generation;
--  with Clortho.Password_Conditions;
--  with Clortho.Password_Style;
with Clortho.Command_Line;
with Clortho.Logging;
with Clortho.Commands;
with Clortho.Command_Runners;

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

   Exit_Status : Command_Runners.Command_Exit_Status;
begin
   if not Command_Line.Is_Ok (Config) then
      Logging.Print_To_Stderr (Command_Line.Error_Message (Config));
      Set_Exit_Status (Ada.Command_Line.Failure);
      return;
   end if;

   case Command_Line.Command (Config) is
      when Commands.Get_Password =>
         Command_Runners.Get_Password (Config, Exit_Status);

      when Commands.Create_Entry =>
         Command_Runners.Create_Entry (Config, Exit_Status);

      when Commands.Renew_Password =>
         Command_Runners.Renew_Password (Config, Exit_Status);

      when Commands.Vacuum_Entry =>
         Command_Runners.Vacuum_Entry (Config, Exit_Status);

      when Commands.Roll_Back_Entry =>
         Command_Runners.Roll_Back_Entry (Config, Exit_Status);

      when Commands.Delete_Entry =>
         Command_Runners.Delete_Entry (Config, Exit_Status);

      when Commands.Vacuum_All =>
         Command_Runners.Vacuum_All (Config, Exit_Status);
   end case;

   if not Command_Runners.Is_Ok (Exit_Status) then
      Logging.Print_To_Stderr (Command_Runners.Error_Message (Exit_Status));
      Set_Exit_Status (Ada.Command_Line.Failure);
   else
      Set_Exit_Status (Ada.Command_Line.Success);
   end if;

end Clortho.Main;
