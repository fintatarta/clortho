pragma Ada_2012;
with Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
with Hidden_Input;

with Clortho.Agent_Channels;
with Clortho.Agent_Communication;  use Clortho.Agent_Communication;
with Clortho.Agent_Protocol_Data;
with Clortho.Utilities;

package body Clortho.DB_Keys is

   ---------
   -- Get --
   ---------

   function Get return DB_Key_Type is
      Channel : Agent_Channels.Client_Channel;
      Reply   : Agent_Protocol_Data.Agent_Reply;
   begin
      Open_Client (Channel);
      Write (Channel, Agent_Protocol_Data.Gimme_Key);
      Read (Channel, Reply);
      Close (Channel);

      return Agent_Protocol_Data.Key (Reply);
   end Get;

   -------------------
   -- Is_Agent_Call --
   -------------------

   function Is_Agent_Call return Boolean is
      Agent_Name : constant String := "clortho-agent";
   begin
      return Utilities.Ends_With (Item => Ada.Command_Line.Command_Name,
                                  Tail => Agent_Name);
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
         Db_Key : constant DB_Key_Type :=
                    Load_Encrypted_File ("db_key.enc", Passphrase (1 .. Last));

         Channel : Agent_Channels.Agent_Channel;
         Request : Agent_Protocol_Data.Agent_Command;
      begin
         Open_Agent (Channel);
         Publish (Channel);
         Detach_Yourself;

         loop
            Read (Channel, Request);

            case Agent_Protocol_Data.Action (Request) is
               when Agent_Protocol_Data.Get_Key =>
                  Write (Channel, Agent_Protocol_Data.Key_Data (Db_Key));

               when Agent_Protocol_Data.Bye =>
                  Close (Channel);
                  return;
            end case;
         end loop;
      end;
   end Work_As_Agent;

end Clortho.DB_Keys;
