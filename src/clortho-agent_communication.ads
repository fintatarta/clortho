with Clortho.Agent_Protocol_Data;   use Clortho.Agent_Protocol_Data;
with Clortho.Agent_Channels;        use Clortho.Agent_Channels;

package Clortho.Agent_Communication is
   procedure Open (Channel : in out Agent_Channel);

   procedure Close (Channel : in out Agent_Channel);

   procedure Publish (Channel : Agent_Channel);

   procedure Detach_Yourself;

   function Read (From : Agent_Channel) return Agent_Command;

   procedure Write (To   : Agent_Channel;
                    What : Agent_Reply);
end Clortho.Agent_Communication;
