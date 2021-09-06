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

   --  -------------------
   --  -- Is_Agent_Call --
   --  -------------------
   --
   --  function Is_Agent_Call return Boolean is
   --     Agent_Name : constant String := "clortho-agent";
   --  begin
   --     return Utilities.Ends_With (Item => Ada.Command_Line.Command_Name,
   --                                 Tail => Agent_Name);
   --  end Is_Agent_Call;

   --  -------------------
   --  -- Work_As_Agent --
   --  -------------------
   --
   --  procedure Work_As_Agent is
   --     Passphrase : String (1 .. 1024);
   --     Last : Natural;
   --  begin
   --   nd Work_As_Agent;

end Clortho.DB_Keys;
