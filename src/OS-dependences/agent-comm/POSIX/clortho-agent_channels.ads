with GNAT.Sockets;
with Ada.Strings.Unbounded;          use Ada.Strings.Unbounded;

package Clortho.Agent_Channels is
   type Agent_Channel is
      record
         Server : GNAT.Sockets.Socket_Type;
         Sock   : GNAT.Sockets.Socket_Type;
         Name   : Unbounded_String;
         Stream : GNAT.Sockets.Stream_Access;
      end record;

   type Client_Channel is
      record
         Sock   : GNAT.Sockets.Socket_Type;
         Stream : GNAT.Sockets.Stream_Access;
      end record;
end Clortho.Agent_Channels;
