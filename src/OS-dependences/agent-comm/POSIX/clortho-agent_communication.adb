pragma Ada_2012;
with Ada.Strings.Unbounded;          use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with GNAT.Sockets;                   use GNAT.Sockets;
with Interfaces.C;

package body Clortho.Agent_Communication is

   function Socket_Name return String
   is ("/tmp/gigi");

   ----------
   -- Open --
   ----------

   procedure Open_Agent (Channel : in out Agent_Channel) is
   begin
      Create_Socket (Channel.Server);

      Channel.Name := To_Unbounded_String (Socket_Name);
      Channel.Stream := null;

      Bind_Socket (Socket  => Channel.Server,
                   Address => Unix_Socket_Address (Socket_Name));

      Listen_Socket (Channel.Server);
   end Open_Agent;

   -----------------
   -- Open_Client --
   -----------------

   procedure Open_Client (Channel : in out Client_Channel)
   is
   begin
      Create_Socket (Channel.Sock);

      Connect_Socket (Socket => Channel.Sock,
                      Server => Unix_Socket_Address (Socket_Name));

      Channel.Stream := Stream (Channel.Sock);

   end Open_Client;

   -----------
   -- Close --
   -----------

   procedure Close (Channel : in out Agent_Channel) is
   begin
      Close_Socket (Channel.Server);
   end Close;

   -----------
   -- Close --
   -----------

   procedure Close (Channel : in out Client_Channel) is
   begin
      Close_Socket (Channel.Sock);
   end Close;

   -------------
   -- Publish --
   -------------

   procedure Publish (Channel : Agent_Channel) is
   begin
      Put_Line ("export CLORTHO_SOCK='" & To_String (Channel.Name) & "'");
   end Publish;

   ---------------------
   -- Detach_Yourself --
   ---------------------

   procedure Detach_Yourself is
      use Interfaces.C;

      procedure Daemon (No_Change_Dir : int;
                        No_Close      : int)
        with
          Import,
          Convention => C,
          Global => null,
          External_Name => "daemon";
   begin
      Daemon (0, 0);
   end Detach_Yourself;

   ----------
   -- Read --
   ----------

   procedure Read (From    : in out Agent_Channel;
                   Command : out Agent_Command)
   is
      Address : Sock_Addr_Type;
   begin
      Accept_Socket (Server  => From.Server,
                     Socket  => From.Sock,
                     Address => Address);

      From.Stream := Stream (From.Sock);

      Command := Agent_Command'Input (From.Stream);
   end Read;

   ----------
   -- Read --
   ----------

   procedure Read (From  : in out Client_Channel;
                   Reply : out Agent_Reply)
   is
   begin
      Reply := Agent_Reply'Input (From.Stream);
   end Read;

   -----------
   -- Write --
   -----------

   procedure Write (To    : in out Agent_Channel;
                    Reply : Agent_Reply) is
   begin
      Agent_Reply'Output (To.Stream, Reply);

      Close_Socket (To.Sock);
      To.Stream := null;
   end Write;
   -----------
   -- Write --
   -----------

   procedure Write (To : in out Client_Channel; What : Agent_Command) is
   begin
      Agent_Command'Output (To.Stream, What);
   end Write;

end Clortho.Agent_Communication;
