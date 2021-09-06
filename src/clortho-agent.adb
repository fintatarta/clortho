pragma Ada_2012;

with Ada.Text_IO;

with Clortho.Agent_Channels;
with Clortho.Agent_Protocol_Data;
with Clortho.Agent_Communication;
with Clortho.DB_Keys;
with Clortho.Configuration;
with Hidden_Input;

procedure Clortho.Agent is
   pragma SPARK_Mode;

   Passphrase : String (1 .. 1024);
   Last : Natural;
begin
   Ada.Text_IO.Put ("Passphrase: ");
   Hidden_Input.Get (Result => Passphrase,
                     Last   => Last);

   declare
      use Clortho.Agent_Communication;

      Db_Key : constant DB_Keys.DB_Key_Type :=
                 Load_Encrypted_File (Filename => Configuration.Key_Filename,
                                      Passphrase => Passphrase (1 .. Last));

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
end Clortho.Agent;
