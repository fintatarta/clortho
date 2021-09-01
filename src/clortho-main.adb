--  with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;

--  with Clortho.Password_Generation;
--  with Clortho.Password_Conditions;
--  with Clortho.Password_Style;
with Clortho.Command_Line;
with Clortho.Logging;

procedure Clortho.Main is

pragma SPARK_Mode;

   --  Conditions : constant Password_Conditions.Condition_Type :=
   --                 Password_Style.Parse (Input       => "/a-z/A-Z/0-9/!@\/",
   --                                       Missing_Are => Password_Style.Prohibited);
   Config : constant Clortho.Command_Line.Parsed_CLI :=
              Clortho.Command_Line.Parse_Command_Line;
begin
   if not Command_Line.Is_Ok (Config) then
      Logging.Print_To_Stderr (Command_Line.Error_Message (Config));
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      return;
   end if;

   case Command_Line.Command (Config) is
      when Command_Line.Get_Password =>
         null;

      when Command_Line.Get_Old_Password =>
         null;

      when Command_Line.Create_Entry =>
         null;

      when Command_Line.Renew_Password =>
         null;

      when Command_Line.Vacuum_Entry =>
         null;

      when Command_Line.Roll_Back_Entry =>
         null;

      when Command_Line.Delete_Entry =>
         null;

      when Command_Line.Vacuum_All =>
         null;
   end case;
   --  Put_Line (Password_Generation.Get_Password (Length     => 12,
   --                                              Constraint => Conditions));
end Clortho.Main;
