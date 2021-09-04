pragma Ada_2012;
with Ada.Numerics.Elementary_Functions;
with Ada.Strings.Maps;
with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;

with Clortho.Command_Line;   use Clortho.Command_Line;
with Clortho.Flagged_Types;  use Clortho.Flagged_Types;
with Clortho.Option_Sets;
with Clortho.Password_Style;
with Clortho.Password_Generation;
with Clortho.Password_Conditions;
with Clortho.Password_Targets;

with Clortho.Db;             use Clortho.Db;

package body Clortho.Command_Runners is

   Default_Specs           : constant String := "/a-z/A-Z/0-9/^a-zA-Z0-9/";
   Default_Password_Length : constant Positive := 12;

   function Get_New_Password (Config : Command_Line.Parsed_CLI)
                              return String_Status
     with
       Pre =>
         Command (Config) = Create_Entry
         or Command (Config) = Renew_Password;

   function No_Password_Options (Config : Command_Line.Parsed_CLI)
                                 return Boolean
     with
       Pre =>
         not (Command (Config) = Create_Entry
              or Command (Config) = Renew_Password);

   function Unexpected_Password_Option (Config : Command_Line.Parsed_CLI)
                                        return Exit_Status;

   function No_Password_Options (Config : Command_Line.Parsed_CLI)
                                 return Boolean
   is (not
         (User_Provided_Password (Config)
          or Option_Sets.Is_Defined (Password_Spec (Config))
          or Option_Sets.Is_Defined (Password_Length (Config))
          or Option_Sets.Is_Defined (Password_Nbits (Config))));

   function Get_New_Password (Config : Command_Line.Parsed_CLI)
                              return String_Status
   is
      function Bits_To_Char (Entropy : Positive;
                             Specs   : Password_Conditions.Condition_Type)
                             return Positive;

      function Bits_To_Char (Entropy : Positive;
                             Specs   : Password_Conditions.Condition_Type)
                             return Positive
      is
         use Password_Conditions;
         use Ada.Numerics.Elementary_Functions;
         use Ada.Strings.Maps;

         Allowed      : constant String := To_Sequence (Allowed_Chars (Specs));
         Bit_Per_Char : constant Float :=
                          Log (Float (Allowed'Length), 2.0);
      begin
         return Positive (Float'Ceiling (Float (Entropy) / Bit_Per_Char));
      end Bits_To_Char;
   begin
      if User_Provided_Password (Config) then
         return (S      => To_Unbounded_String (User_Password (Config)),
                 Status => Success);
      end if;

      declare
         use type Password_Style.Exit_Status;

         package PG renames Password_Generation;
         package CL renames Command_Line;

         Specs        : constant String :=
                          (if Option_Sets.Is_Defined (Password_Spec (Config)) then
                              Value (Password_Spec (Config))
                           else
                              Default_Specs);

         Parsed_Specs : constant Password_Style.Parsing_Result :=
                          Password_Style.Parse
                            (Input       => Specs,
                             Missing_Are => Password_Style.Prohibited);

         Length : Positive;

      begin
         if Parsed_Specs.Status /= Password_Style.Ok then
            return (S      => Null_Unbounded_String,
                    Status => Failure ("Bad spec syntax"));

         else
            pragma Assert (Parsed_Specs.Status = Password_Style.Ok);

            if Option_Sets.Is_Defined (Password_Length (Config)) then
               Length := Option_Sets.Value (CL.Password_Length (Config));

            elsif Option_Sets.Is_Defined (CL.Password_Nbits (Config)) then
               Length := Bits_To_Char
                 (Entropy => Option_Sets.Value (CL.Password_Nbits (Config)),
                  Specs   => Parsed_Specs.Conditions);

            else
               Length := Default_Password_Length;
            end if;

            return (S      =>
                      To_Unbounded_String
                        (PG.Get_Password (Length, Parsed_Specs.Conditions)),
                    Status => Success);
         end if;
      end;
   end Get_New_Password;

   function Unexpected_Password_Option (Config : Command_Line.Parsed_CLI)
                                        return Exit_Status
   is (Failure ("Unexpected password-related option"));

   ------------------
   -- Get_Password --
   ------------------

   procedure Get_Password (Config         : Command_Line.Parsed_CLI;
                           Db             : in out Clortho.Db.Password_Db;
                           Command_Status : out Exit_Statuses.Exit_Status)
   is
   begin
      if not No_Password_Options (Config) then
         Command_Status := Unexpected_Password_Option (Config);
         return;
      end if;

      declare
         use Password_Targets;

         Key : constant String := Entry_Key (Config);
         Ver : constant Positive := Requested_Version (Config);
      begin
         if not Contains (Db, Key) then
            Command_Status := Failure ("Name not found");
            return;
         end if;

         if N_Versions (Db, Key) < Ver then
            Command_Status := Failure ("Version not found");
            return;
         end if;

         Send_Password_To (Password => Get_Password (Db, Key, Ver),
                           Target   => Target (Config),
                           Status   => Command_Status);
      end;
   end Get_Password;

   ------------------
   -- Create_Entry --
   ------------------

   procedure Create_Entry (Config         : Command_Line.Parsed_CLI;
                           Db             : in out Clortho.Db.Password_Db;
                           Command_Status : out Exit_Status) is
      New_Password : constant String_Status := Get_New_Password (Config);
   begin
      if not Is_Success (New_Password.Status) then
         Command_Status := New_Password.Status;
         return;
      end if;
   end Create_Entry;

   --------------------
   -- Renew_Password --
   --------------------

   procedure Renew_Password (Config         : Command_Line.Parsed_CLI;
                             Db             : in out Clortho.Db.Password_Db;
                             Command_Status : out Exit_Status)
   is
      New_Password : constant String_Status := Get_New_Password (Config);
   begin
      if not Is_Success (New_Password.Status) then
         Command_Status := New_Password.Status;
         return;
      end if;
   end Renew_Password;

   ------------------
   -- Vacuum_Entry --
   ------------------

   procedure Vacuum_Entry (Config         : Command_Line.Parsed_CLI;
                           Db             : in out Clortho.Db.Password_Db;
                           Command_Status : out Exit_Status) is
   begin
      if not No_Password_Options (Config) then
         Command_Status := Unexpected_Password_Option (Config);
         return;
      end if;

      pragma Compile_Time_Warning
        (Standard.True, "Vacuum_Entry unimplemented");
      raise Program_Error with "Unimplemented procedure Vacuum_Entry";
   end Vacuum_Entry;

   ---------------------
   -- Roll_Back_Entry --
   ---------------------

   procedure Roll_Back_Entry (Config         : Command_Line.Parsed_CLI;
                              Db             : in out Clortho.Db.Password_Db;
                              Command_Status : out Exit_Status) is
   begin
      if not No_Password_Options (Config) then
         Command_Status := Unexpected_Password_Option (Config);
         return;
      end if;

      pragma Compile_Time_Warning
        (Standard.True, "Roll_Back_Entry unimplemented");
      raise Program_Error with "Unimplemented procedure Roll_Back_Entry";
   end Roll_Back_Entry;

   ------------------
   -- Delete_Entry --
   ------------------

   procedure Delete_Entry (Config         : Command_Line.Parsed_CLI;
                           Db             : in out Clortho.Db.Password_Db;
                           Command_Status : out Exit_Status) is
   begin
      if not No_Password_Options (Config) then
         Command_Status := Unexpected_Password_Option (Config);
         return;
      end if;

      pragma Compile_Time_Warning
        (Standard.True, "Delete_Entry unimplemented");
      raise Program_Error with "Unimplemented procedure Delete_Entry";
   end Delete_Entry;

   ----------------
   -- Vacuum_All --
   ----------------

   procedure Vacuum_All (Config         : Command_Line.Parsed_CLI;
                         Db             : in out Clortho.Db.Password_Db;
                         Command_Status : out Exit_Status)
   is
   begin
      if not No_Password_Options (Config) then
         Command_Status := Unexpected_Password_Option (Config);
         return;
      end if;
      pragma Compile_Time_Warning (Standard.True, "Vacuum_All unimplemented");
      raise Program_Error with "Unimplemented procedure Vacuum_All";
   end Vacuum_All;

   ----------
   -- List --
   ----------

   procedure List (Config         : Command_Line.Parsed_CLI;
                   Db             : in out Clortho.Db.Password_Db;
                   Command_Status : out Exit_Status)
   is
   begin
      if not No_Password_Options (Config) then
         Command_Status := Unexpected_Password_Option (Config);
         return;
      end if;
      pragma Compile_Time_Warning (Standard.True, "Vacuum_All unimplemented");
      raise Program_Error with "Unimplemented procedure Vacuum_All";
   end List;
end Clortho.Command_Runners;
