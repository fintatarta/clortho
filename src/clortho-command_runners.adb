pragma Ada_2012;
with Ada.Numerics.Elementary_Functions;
with Ada.Strings.Maps;

with Clortho.Command_Line;   use Clortho.Command_Line;
with Clortho.Flagged_Types;  use Clortho.Flagged_Types;
with Clortho.Option_Sets;
with Clortho.Password_Style;
with Clortho.Password_Generation;
with Clortho.Password_Conditions;

package body Clortho.Command_Runners is

   Default_Specs : constant String := "/a-z/A-Z/0-9/^a-zA-Z0-9/";
   Default_Password_Length : constant Positive := 12;

   Void_Password : constant String := "";

   function Is_Ok (Status : Command_Exit_Status) return Boolean
   is (Status.Err = Ok);

   function Error_Message (Status : Command_Exit_Status) return String
   is (case Status.Err is
          when Ok                         =>
             "No error",

          when Unexpected_Password_Option =>
             "Unexpected password-related options",

          when Generic_Error              =>
             "Generic Error (?)");

   function Get_New_Password (Config : Command_Line.Parsed_CLI) return String
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
                                        return Command_Exit_Status;

   function No_Password_Options (Config : Command_Line.Parsed_CLI)
                                 return Boolean
   is (not
         (User_Provided_Password (Config)
          or Option_Sets.Is_Defined (Password_Spec (Config))
          or Option_Sets.Is_Defined (Password_Length (Config))
          or Option_Sets.Is_Defined (Password_Nbits (Config))));

   function Get_New_Password (Config : Command_Line.Parsed_CLI) return String
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

         Allowed : constant String := To_Sequence (Allowed_Chars (Specs));
         Bit_Per_Char : constant Float :=
                          Log (Float (Allowed'Length), 2.0);
      begin
         return Positive (Float'Ceiling (Float (Entropy) / Bit_Per_Char));
      end Bits_To_Char;
   begin
      if User_Provided_Password (Config) then
         return User_Password (Config);
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
            return Void_Password;

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

            return PG.Get_Password (Length     => Length,
                                    Constraint => Parsed_Specs.Conditions);
         end if;
      end;
   end Get_New_Password;

   function Unexpected_Password_Option (Config : Command_Line.Parsed_CLI)
                                        return Command_Exit_Status
   is ((Err => Unexpected_Password_Option));

   ------------------
   -- Get_Password --
   ------------------

   procedure Get_Password (Config         : Command_Line.Parsed_CLI;
                           Command_Status : out Command_Exit_Status) is
   begin
      if not No_Password_Options (Config) then
         Command_Status := Unexpected_Password_Option (Config);
         return;
      end if;
   end Get_Password;

   ------------------
   -- Create_Entry --
   ------------------

   procedure Create_Entry (Config         : Command_Line.Parsed_CLI;
                           Command_Status : out Command_Exit_Status) is
      New_Password : constant String := Get_New_Password (Config);
   begin
      if New_Password = Void_Password then
         Command_Status := (Err => Generic_Error);
         return;
      end if;
   end Create_Entry;

   --------------------
   -- Renew_Password --
   --------------------

   procedure Renew_Password (Config         : Command_Line.Parsed_CLI;
                             Command_Status : out Command_Exit_Status)
   is
      New_Password : constant String := Get_New_Password (Config);
   begin
      if New_Password = Void_Password then
         Command_Status := (Err => Generic_Error);
         return;
      end if;
   end Renew_Password;

   ------------------
   -- Vacuum_Entry --
   ------------------

   procedure Vacuum_Entry (Config         : Command_Line.Parsed_CLI;
                           Command_Status : out Command_Exit_Status) is
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
                              Command_Status : out Command_Exit_Status) is
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
                           Command_Status : out Command_Exit_Status) is
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
                         Command_Status : out Command_Exit_Status)
   is
   begin
      if not No_Password_Options (Config) then
         Command_Status := Unexpected_Password_Option (Config);
         return;
      end if;
      pragma Compile_Time_Warning (Standard.True, "Vacuum_All unimplemented");
      raise Program_Error with "Unimplemented procedure Vacuum_All";
   end Vacuum_All;

end Clortho.Command_Runners;
