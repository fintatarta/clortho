pragma Ada_2012;
package body Clortho.Option_Sets is

   -----------------------
   -- Set_Default_Value --
   -----------------------

   procedure Set_Default_Value (Item : out Option_Set) is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Set_Default_Value unimplemented");
      raise Program_Error with "Unimplemented procedure Set_Default_Value";
   end Set_Default_Value;

   ------------
   -- Action --
   ------------

   function Action (Item : Option_Set) return Flagged_Command is
   begin
      pragma Compile_Time_Warning (Standard.True, "Action unimplemented");
      return raise Program_Error with "Unimplemented function Action";
   end Action;

   ------------
   -- Action --
   ------------

   function Action (Item : Option_Set) return Command_Type is
   begin
      pragma Compile_Time_Warning (Standard.True, "Action unimplemented");
      return raise Program_Error with "Unimplemented function Action";
   end Action;

   ---------------------
   -- Do_Get_Password --
   ---------------------

   procedure Do_Get_Password (Item : in out Option_Set; N : Natural) is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Do_Get_Password unimplemented");
      raise Program_Error with "Unimplemented procedure Do_Get_Password";
   end Do_Get_Password;

   ----------------------
   -- Password_Version --
   ----------------------

   function Password_Version (Item : Option_Set) return Natural is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Password_Version unimplemented");
      return
        raise Program_Error with "Unimplemented function Password_Version";
   end Password_Version;

   ---------------------
   -- Do_Create_Entry --
   ---------------------

   procedure Do_Create_Entry (Item : in out Option_Set) is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Do_Create_Entry unimplemented");
      raise Program_Error with "Unimplemented procedure Do_Create_Entry";
   end Do_Create_Entry;

   -----------------------
   -- Do_Renew_Password --
   -----------------------

   procedure Do_Renew_Password (Item : in out Option_Set) is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Do_Renew_Password unimplemented");
      raise Program_Error with "Unimplemented procedure Do_Renew_Password";
   end Do_Renew_Password;

   -------------
   -- Do_List --
   -------------

   procedure Do_List (Item : in out Option_Set) is
   begin
      pragma Compile_Time_Warning (Standard.True, "Do_List unimplemented");
      raise Program_Error with "Unimplemented procedure Do_List";
   end Do_List;

   ---------------
   -- Do_Vacuum --
   ---------------

   procedure Do_Vacuum (Item : in out Option_Set) is
   begin
      pragma Compile_Time_Warning (Standard.True, "Do_Vacuum unimplemented");
      raise Program_Error with "Unimplemented procedure Do_Vacuum";
   end Do_Vacuum;

   --------------------
   -- Do_Full_Vacuum --
   --------------------

   procedure Do_Full_Vacuum (Item : in out Option_Set) is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Do_Full_Vacuum unimplemented");
      raise Program_Error with "Unimplemented procedure Do_Full_Vacuum";
   end Do_Full_Vacuum;

   ---------------
   -- Do_Delete --
   ---------------

   procedure Do_Delete (Item : in out Option_Set) is
   begin
      pragma Compile_Time_Warning (Standard.True, "Do_Delete unimplemented");
      raise Program_Error with "Unimplemented procedure Do_Delete";
   end Do_Delete;

   ------------------
   -- Do_Roll_Back --
   ------------------

   procedure Do_Roll_Back (Item : in out Option_Set) is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Do_Roll_Back unimplemented");
      raise Program_Error with "Unimplemented procedure Do_Roll_Back";
   end Do_Roll_Back;

   -------------------
   -- User_Password --
   -------------------

   function User_Password (Item : Option_Set) return Flagged_String is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "User_Password unimplemented");
      return raise Program_Error with "Unimplemented function User_Password";
   end User_Password;

   -----------------------
   -- Set_User_Password --
   -----------------------

   procedure Set_User_Password
     (Item : in out Option_Set; Password : Unbounded_String)
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Set_User_Password unimplemented");
      raise Program_Error with "Unimplemented procedure Set_User_Password";
   end Set_User_Password;

   --------------------
   -- Password_Nbits --
   --------------------

   function Password_Nbits (Item : Option_Set) return Entropy is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Password_Nbits unimplemented");
      return raise Program_Error with "Unimplemented function Password_Nbits";
   end Password_Nbits;

   ---------------------
   -- Password_Nchars --
   ---------------------

   function Password_Nchars (Item : Option_Set) return Char_Length is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Password_Nchars unimplemented");
      return raise Program_Error with "Unimplemented function Password_Nchars";
   end Password_Nchars;

   ------------------------
   -- Set_Password_Nbits --
   ------------------------

   procedure Set_Password_Nbits (Item : in out Option_Set; N_Bits : Positive)
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Set_Password_Nbits unimplemented");
      raise Program_Error with "Unimplemented procedure Set_Password_Nbits";
   end Set_Password_Nbits;

   -------------------------
   -- Set_Password_Nchars --
   -------------------------

   procedure Set_Password_Nchars (Item : in out Option_Set; N_Chars : Positive)
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Set_Password_Nchars unimplemented");
      raise Program_Error with "Unimplemented procedure Set_Password_Nchars";
   end Set_Password_Nchars;

   -------------------
   -- Password_Spec --
   -------------------

   function Password_Spec (Item : Option_Set) return Flagged_String is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Password_Spec unimplemented");
      return raise Program_Error with "Unimplemented function Password_Spec";
   end Password_Spec;

   ------------------------
   -- Set_Password_Specs --
   ------------------------

   procedure Set_Password_Specs
     (Item : in out Option_Set; Specs : Unbounded_String)
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Set_Password_Specs unimplemented");
      raise Program_Error with "Unimplemented procedure Set_Password_Specs";
   end Set_Password_Specs;

   ------------
   -- Source --
   ------------

   function Source (Item : Option_Set) return Source_Name is
   begin
      pragma Compile_Time_Warning (Standard.True, "Source unimplemented");
      return raise Program_Error with "Unimplemented function Source";
   end Source;

   ----------------
   -- Use_Source --
   ----------------

   procedure Use_Source (Item : in out Option_Set; Source : Source_Name) is
   begin
      pragma Compile_Time_Warning (Standard.True, "Use_Source unimplemented");
      raise Program_Error with "Unimplemented procedure Use_Source";
   end Use_Source;

   ------------
   -- Target --
   ------------

   function Target (Item : Option_Set) return Target_Name is
   begin
      pragma Compile_Time_Warning (Standard.True, "Target unimplemented");
      return raise Program_Error with "Unimplemented function Target";
   end Target;

   ----------------
   -- Use_Target --
   ----------------

   procedure Use_Target (Item : in out Option_Set; Target : Target_Name) is
   begin
      pragma Compile_Time_Warning (Standard.True, "Use_Target unimplemented");
      raise Program_Error with "Unimplemented procedure Use_Target";
   end Use_Target;

end Clortho.Option_Sets;
