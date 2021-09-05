with Clortho.Agent_Protocol_Data;   use Clortho.Agent_Protocol_Data;
with Clortho.Agent_Channels;        use Clortho.Agent_Channels;

package Clortho.Agent_Communication is
   procedure Open_Agent (Channel : in out Agent_Channel);

   procedure Open_Client (Channel : in out Client_Channel);

   procedure Close (Channel : in out Agent_Channel);

   procedure Close (Channel : in out Client_Channel);

   procedure Publish (Channel : Agent_Channel);

   procedure Detach_Yourself;

   procedure Read (From    : in out Agent_Channel;
                   Command : out Agent_Command);

   procedure Write (To    : in out Agent_Channel;
                    Reply : Agent_Reply);

   procedure Read (From    : in out Client_Channel;
                   Reply   : out Agent_Reply);

   procedure Write (To   : in out Client_Channel;
                    What : Agent_Command);
end Clortho.Agent_Communication;
