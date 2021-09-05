with GNAT.Sockets;

package Clortho.Agent_Channels is
   subtype Agent_Channel is GNAT.Sockets.Socket_Type;

   subtype Client_Channel is GNAT.Sockets.Socket_Type;
end Clortho.Agent_Channels;
