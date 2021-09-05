with Clortho.Agent_Protocol_Data;   use Clortho.Agent_Protocol_Data;

package Clortho.Agent_Communication is
   type Agent_Channel is private;

   procedure Open (Channel : in out Agent_Channel);

   procedure Close (Channel : in out Agent_Channel);

   procedure Publish (Channel : Agent_Channel);

   procedure Detach_Yourself;

   function Read (From : Agent_Channel) return Agent_Command;

   procedure Write (To   : Agent_Channel;
                    What : Agent_Reply);

private
   type Agent_Channel is null record;

end Clortho.Agent_Communication;
