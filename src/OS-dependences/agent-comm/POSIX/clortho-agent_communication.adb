pragma Ada_2012;
with GNAT.Sockets;
with Clortho.Agent_Channels;    use Clortho.Agent_Channels;

package body Clortho.Agent_Communication is

   function Socket_Name return String
   is ("/tmp/gigi");

   ----------
   -- Open --
   ----------

   procedure Open_Agent (Channel : in out Agent_Channel) is
      use GNAT.Sockets;

      Addr : constant Sock_Addr_Type := Unix_Socket_Address (Socket_Name);
   begin
      Create_Socket (Channel);

      Bind_Socket (Socket  => Channel,
                   Address => Addr);

      Listen_Socket (Channel);
   end Open_Agent;

   -----------------
   -- Open_Client --
   -----------------

   procedure Open_Client (Channel : in out Agent_Channel)
   is
   begin
      pragma Compile_Time_Warning (Standard.True, "Close unimplemented");
      raise Program_Error with "Unimplemented procedure Close";
   end Open_Client;

   -----------
   -- Close --
   -----------

   procedure Close (Channel : in out Agent_Channel) is
   begin
      pragma Compile_Time_Warning (Standard.True, "Close unimplemented");
      raise Program_Error with "Unimplemented procedure Close";
   end Close;

   -------------
   -- Publish --
   -------------

   procedure Publish (Channel : Agent_Channel) is
   begin
      pragma Compile_Time_Warning (Standard.True, "Publish unimplemented");
      raise Program_Error with "Unimplemented procedure Publish";
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

   function Read (From : Agent_Channel) return Agent_Command is
   begin
      pragma Compile_Time_Warning (Standard.True, "Read unimplemented");
      return raise Program_Error with "Unimplemented function Read";
   end Read;

   -----------
   -- Write --
   -----------

   procedure Write (To : Agent_Channel; What : Agent_Reply) is
   begin
      pragma Compile_Time_Warning (Standard.True, "Write unimplemented");
      raise Program_Error with "Unimplemented procedure Write";
   end Write;

end Clortho.Agent_Communication;
