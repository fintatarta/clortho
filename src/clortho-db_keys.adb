pragma Ada_2012;
with Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
with Hidden_Input;

with Clortho.Agent_Communication;

package body Clortho.DB_Keys is

   ---------
   -- Get --
   ---------

   function Get return DB_Key_Type is
   begin
      pragma Compile_Time_Warning (Standard.True, "Get unimplemented");
      return raise Program_Error with "Unimplemented function Get";
   end Get;

   -------------------
   -- Is_Agent_Call --
   -------------------

   function Is_Agent_Call return Boolean is
      Command    : constant String := Ada.Command_Line.Command_Name;
      Agent_Name : constant String := "clortho-agent";
   begin
      if Command'Length < Agent_Name'Length then
         return False;
      end if;

      pragma Assert (Command'Length > 0);

      declare
         From : constant Positive := Command'Last - Agent_Name'Length + 1;
         S    : constant String := Command (From .. Command'Last) with Ghost;
      begin
         pragma Assert (S'Length = Agent_Name'Length);

         return Command (From .. Command'Last) = Agent_Name;
      end;
   end Is_Agent_Call;

   -------------------
   -- Work_As_Agent --
   -------------------

   procedure Work_As_Agent is
      Passphrase : String (1 .. 1024);
      Last : Natural;
   begin
      Put ("Passphrase: ");
      Hidden_Input.Get (Result => Passphrase,
                        Last   => Last);

      declare
         use Agent_Communication;

         Db_Key : constant DB_Key_Type :=
                    Load_Encrypted_File ("db_key.enc", Passphrase (1 .. Last));

         Channel : Agent_Channel;
      begin
         Open (Channel);
         Publish (Channel);
         Detach_Yourself;

         loop
            case Action (Read (Channel)) is
               when Get_Key =>
                  Write (Channel, Key_Data (Db_Key));

               when Bye =>
                  Close (Channel);
                  return;
            end case;
         end loop;
      end;
   end Work_As_Agent;

end Clortho.DB_Keys;
