pragma Ada_2012;
package body Clortho.Command_Runners is

   function Is_Ok (Status : Command_Exit_Status) return Boolean
   is (Status.Err = Ok);

   function Error_Message (Status : Command_Exit_Status) return String
   is (case Status.Err is
          when Ok            => "",
          when Generic_Error => "Generic Error (?)");

   ------------------
   -- Get_Password --
   ------------------

   procedure Get_Password (Config         : Command_Line.Parsed_CLI;
                           Command_Status : out Command_Exit_Status) is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Get_Password unimplemented");
      raise Program_Error with "Unimplemented procedure Get_Password";
   end Get_Password;

   ------------------
   -- Create_Entry --
   ------------------

   procedure Create_Entry (Config         : Command_Line.Parsed_CLI;
                           Command_Status : out Command_Exit_Status) is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Create_Entry unimplemented");
      raise Program_Error with "Unimplemented procedure Create_Entry";
   end Create_Entry;

   --------------------
   -- Renew_Password --
   --------------------

   procedure Renew_Password (Config         : Command_Line.Parsed_CLI;
                             Command_Status : out Command_Exit_Status) is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Renew_Password unimplemented");
      raise Program_Error with "Unimplemented procedure Renew_Password";
   end Renew_Password;

   ------------------
   -- Vacuum_Entry --
   ------------------

   procedure Vacuum_Entry (Config         : Command_Line.Parsed_CLI;
                           Command_Status : out Command_Exit_Status) is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Vacuum_Entry unimplemented");
      raise Program_Error with "Unimplemented procedure Vacuum_Entry";
   end Vacuum_Entry;

   ---------------------
   -- Roll_Back_Entry --
   ---------------------

   procedure Roll_Back_Entry (Config         : Command_Line.Parsed_CLI;
                              Command_Status : out Command_Exit_Status) is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Roll_Back_Entry unimplemented");
      raise Program_Error with "Unimplemented procedure Roll_Back_Entry";
   end Roll_Back_Entry;

   ------------------
   -- Delete_Entry --
   ------------------

   procedure Delete_Entry (Config         : Command_Line.Parsed_CLI;
                           Command_Status : out Command_Exit_Status) is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Delete_Entry unimplemented");
      raise Program_Error with "Unimplemented procedure Delete_Entry";
   end Delete_Entry;

   ----------------
   -- Vacuum_All --
   ----------------

   procedure Vacuum_All (Config         : Command_Line.Parsed_CLI;
                         Command_Status : out Command_Exit_Status) is
   begin
      pragma Compile_Time_Warning (Standard.True, "Vacuum_All unimplemented");
      raise Program_Error with "Unimplemented procedure Vacuum_All";
   end Vacuum_All;

end Clortho.Command_Runners;
