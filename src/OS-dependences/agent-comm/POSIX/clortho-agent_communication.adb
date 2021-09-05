pragma Ada_2012;
with GNAT.Sockets;
with Clortho.Agent_Channels;    use Clortho.Agent_Channels;

package body Clortho.Agent_Communication is

   ----------
   -- Open --
   ----------

   procedure Open (Channel : in out Agent_Channel) is
   begin
      pragma Compile_Time_Warning (Standard.True, "Open unimplemented");
      raise Program_Error with "Unimplemented procedure Open";
   end Open;

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
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Detach_Yourself unimplemented");
      raise Program_Error with "Unimplemented procedure Detach_Yourself";
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
