with Ada.Text_IO; use Ada.Text_IO;
with Clortho.Password_Generation;
with Clortho.Password_Conditions;
with Clortho.Password_Style;

procedure Clortho.Main is
   Conditions : constant Password_Conditions.Condition_Type :=
                  Password_Style.Parse (Input       => "/a-z/A-Z/0-9/!@\/",
                                        Missing_Are => Password_Style.Prohibited);
begin
   Put_Line (Password_Generation.Get_Password (Length     => 12,
                                               Constraint => Conditions));
end Clortho.Main;
