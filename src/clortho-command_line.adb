pragma Ada_2012;
with Ada.Command_Line;
with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;
with Ada.Strings.Fixed;

with Password_Style_Parsers;

package body Clortho.Command_Line is
   pragma SPARK_Mode;

   function "+" (X : String) return Unbounded_String
                 renames To_Unbounded_String;

   type Option_Symbol is
     (
      Old,
      Back,
      Vacuum,
      Full_Vacuum,
      Create,
      Renew,
      Delete,
      Roll_Back,
      User_Password,
      Password_Length,
      Password_Bits,
      Password_Spec,
      Input,
      Output,
      Filter,
      End_Of_Options,
      Unknown_Option,
      Missing_Parameter,
      Unrequested_Parameter,
      Bad_Option_Syntax
     );

   --   subtype Error is Option_Symbol range Unknown_Option .. Bad_Option_Syntax;

   Need_Parameter : constant array (Option_Symbol) of Boolean :=
                      (
                       Old                   => False,
                       Back                  => True,
                       Vacuum                => False,
                       Full_Vacuum           => False,
                       Create                => False,
                       Renew                 => False,
                       User_Password         => True,
                       Password_Length       => True,
                       Password_Bits         => True,
                       Password_Spec         => True,
                       Roll_Back             => False,
                       Delete                => False,
                       Input                 => False,
                       Output                => False,
                       Filter                => False,
                       End_Of_Options        => False,
                       Unknown_Option        => False,
                       Missing_Parameter     => False,
                       Unrequested_Parameter => False,
                       Bad_Option_Syntax     => False
                      );

   type Option_Descriptor is
      record
         Name           : Unbounded_String;
         Symbol         : Option_Symbol;
      end record;

   type Descriptor_Array is array (Positive range <>) of Option_Descriptor;

   Options : constant Descriptor_Array :=
               ((+"old", Old),
                (+"back", Back),
                (+"vacuum", Vacuum),
                (+"scurdammoce-o-passato", Full_Vacuum),
                (+"create", Create),
                (+"renew", Renew),
                (+"password", User_Password),
                (+"len", Password_Length),
                (+"length", Password_Length),
                (+"size", Password_Length),
                (+"nbit", Password_Bits),
                (+"charset", Password_Spec),
                (+"spec", Password_Spec),
                (+"roll-back", Roll_Back),
                (+"undo", Roll_Back),
                (+"delete-full-entry", Delete),
                (+"in", Input),
                (+"out", Output),
                (+"filter", Filter),
                (+"in-out", Filter),
                (+"inout", Filter)
                --  (+"not-smart", Non_Url_Matching),
                --  (+"strict", Non_Url_Matching)
               );

   ------------------------
   -- Parse_Command_Line --
   ------------------------

   function Parse_Command_Line return Parsed_CLI is
      use Ada.Command_Line;

      procedure Start_Option_Scanning (Cursor : out Positive);

      procedure Next_Option (Cursor    : in out Positive;
                             Option    :    out Option_Symbol;
                             Parameter :    out Unbounded_String)
        with
          Pre =>
            Cursor <= Ada.Command_Line.Argument_Count + 1,
            Post =>
              (if not Need_Parameter (Option)
                 then
                   Parameter = Null_Unbounded_String)
              and
                (if Cursor'Old = Ada.Command_Line.Argument_Count + 1
                   then
                     Cursor = Cursor'Old and Option = End_Of_Options);

      procedure Start_Option_Scanning (Cursor : out Positive)
      is
      begin
         Cursor := 1;
      end Start_Option_Scanning;

      procedure Next_Option (Cursor    : in out Positive;
                             Option    :    out Option_Symbol;
                             Parameter :    out Unbounded_String)

      is
         function To_Symbol (X : String) return Option_Symbol;

         procedure Parse_Single_Option (Input     : String;
                                        Option    : out Option_Symbol;
                                        Parameter : out Unbounded_String);

         function To_Symbol (X : String) return Option_Symbol
         is
         begin
            for E of Options loop
               if E.Name = To_Unbounded_String (X) then
                  return E.Symbol;
               end if;
            end loop;

            return Unknown_Option;
         end To_Symbol;

         procedure Parse_Single_Option (Input     : String;
                                        Option    : out Option_Symbol;
                                        Parameter : out Unbounded_String)
         is
            use Ada.Strings.Fixed;

            Equal_Position : constant Natural := Index (Source  => Input,
                                                        Pattern => "=");
         begin
            if Equal_Position = 0 then
               Option := To_Symbol (Input);
               Parameter := Null_Unbounded_String;

               if Need_Parameter (Option) then
                  Option := Missing_Parameter;
               end if;

            elsif
              Equal_Position = Input'First or Equal_Position = Input'Last
            then
               Option := Bad_Option_Syntax;
               Parameter := Null_Unbounded_String;

            else
               declare
                  Name : constant String :=
                           Input (Input'First .. Equal_Position - 1);

                  Value : constant String :=
                            Input (Equal_Position + 1 .. Input'Last);
               begin
                  Option := To_Symbol (Name);

                  if not Need_Parameter (Option) then
                     Option := Unrequested_Parameter;
                     Parameter := Null_Unbounded_String;
                  else
                     Parameter := To_Unbounded_String (Value);
                  end if;
               end;
            end if;
         end Parse_Single_Option;
      begin
         if Cursor = Argument_Count + 1 then
            Option := End_Of_Options;
            Parameter := Null_Unbounded_String;
            return;
         end if;

         pragma Assert (Cursor <= Argument_Count);

         declare
            Arg : constant String := Argument (Cursor);
         begin
            if Arg = "--" then
               Option := End_Of_Options;
               Parameter := Null_Unbounded_String;
               Cursor := Cursor + 1;
               return;
            end if;

            if
              Arg'Length < 3
              or else Arg (Arg'First .. Arg'First + 1) /= "--"
            then
               Option := End_Of_Options;
               Parameter := Null_Unbounded_String;
               return;
            end if;

            pragma Assert (Arg'Length >= 3
                           and Arg (Arg'First .. Arg'First + 1) = "--");

            Parse_Single_Option (Arg (Arg'First + 2 .. Arg'Last), Option, Parameter);
         end;
      end Next_Option;

      function Double_Action return Parsed_CLI;
      function Double_Password return Parsed_CLI;
      function Double_Password_Length return Parsed_CLI;
      function Double_Specs return Parsed_CLI;

      function Bad_Integer (X : Unbounded_String) return Parsed_CLI;

      procedure Parse (Input : String;
                       Value : out Positive;
                       OK    : out Boolean);

      function Double_Action return Parsed_CLI
      is (Parsed_CLI'(Status          => Double_Action,
                      Name_Length     => 0,
                      Password_Length => 0,
                      Explanation     => ""));

      function Bad_Integer (X : Unbounded_String) return Parsed_CLI
      is (Parsed_CLI'(Status          => Bad_Integer,
                      Name_Length     => Length (X),
                      Password_Length => 0,
                      Explanation     => To_String (X)));

      function Double_Password return Parsed_CLI
      is (Parsed_CLI'(Status          => Double_Password,
                      Name_Length     => 0,
                      Password_Length => 0,
                      Explanation     => ""));

      function Double_Specs return Parsed_CLI
      is (Parsed_CLI'(Status          => Double_Specs,
                      Name_Length     => 0,
                      Password_Length => 0,
                      Explanation     => ""));

      function Double_Password_Length return Parsed_CLI
      is (Parsed_CLI'(Status          => Double_Password_Length,
                      Name_Length     => 0,
                      Password_Length => 0,
                      Explanation     => ""));

      procedure Parse (Input : String;
                       Value : out Positive;
                       OK    : out Boolean)
      is
      begin
         OK := (for all C of Input => C in '0' .. '9');
         if OK then
            Value := Positive'Value (Input);
         else
            Value := 1;
         end if;
      end Parse;

      No_User_Password : constant Unbounded_String := Null_Unbounded_String;
      No_Password_Len : constant Natural := 0;
      No_Specs : constant Unbounded_String := Null_Unbounded_String;

      Cursor : Positive;
      Option : Option_Symbol;
      Parameter : Unbounded_String;

      To_Do : Command_Type := Get_Password;
      Back_Step : Natural := 0;
      Password_Len : Natural := No_Password_Len;
      Password_N_Bits : Natural := No_Password_Len;
      User_Provided_Password : Unbounded_String := No_User_Password;
      Output_Target : Target_Name := Clipboard;
      Specs : Unbounded_String := No_Specs;
      Use_Standard_Input : Boolean := False;
   begin
      Start_Option_Scanning (Cursor);

      loop
         Next_Option (Cursor, Option, Parameter);

         exit when Option = End_Of_Options;

         case Option is
            when Old =>
               if To_Do /= Get_Password then
                  return Double_Action;

               else
                  To_Do := Get_Old_Password;
                  Back_Step := 1;

               end if;

            when Back =>
               if To_Do /= Get_Password then
                  return Double_Action;

               else
                  To_Do := Get_Old_Password;
                  declare
                     OK : Boolean;
                  begin
                     Parse (To_String (Parameter), Back_Step, OK);

                     if not OK then
                        return Bad_Integer (Parameter);
                     end if;
                  end;

               end if;

            when Create =>
               if To_Do /= Get_Password then
                  return Double_Action;
               else
                  To_Do := Create_Entry;
               end if;

            when Renew =>
               if To_Do /= Get_Password then
                  return Double_Action;
               else
                  To_Do := Renew_Password;
               end if;

            when Vacuum =>
               if To_Do /= Get_Password then
                  return Double_Action;
               else
                  To_Do := Vacuum_Entry;
               end if;

            when Full_Vacuum =>
               if To_Do /= Get_Password then
                  return Double_Action;
               else
                  To_Do := Vacuum_All;
               end if;

            when Delete =>
               if To_Do /= Get_Password then
                  return Double_Action;
               else
                  To_Do := Delete_Entry;
               end if;

            when Roll_Back =>
               if To_Do /= Get_Password then
                  return Double_Action;
               else
                  To_Do := Roll_Back_Entry;
               end if;

            when  User_Password =>
               if User_Provided_Password = No_User_Password then
                  User_Provided_Password := Parameter;
               else
                  return Double_Password;
               end if;

            when Password_Length =>
               if
                 Password_Len /= No_Password_Len
                 or Password_N_Bits /= No_Password_Len
               then
                  return Double_Password_Length;
               end if;

               declare
                  OK : Boolean;
               begin
                  Parse (To_String (Parameter), Password_Len, OK);

                  if not OK or else Password_Len = 0 then
                     return Bad_Integer (Parameter);
                  end if;
               end;

            when Password_Bits =>
               if
                 Password_Len /= No_Password_Len
                 or Password_N_Bits /= No_Password_Len
               then
                  return Double_Password_Length;
               end if;

               declare
                  OK : Boolean;
               begin
                  Parse (To_String (Parameter), Password_N_Bits, OK);

                  if not OK  then
                     return Bad_Integer (Parameter);
                  end if;
               end;

            when Password_Spec =>
               if Specs /= No_Specs then
                  return Double_Specs;
               end if;

               Specs := Parameter;

            when Input =>
               Use_Standard_Input := True;

            when Output =>
               Output_Target := Standard_Output;

            when Filter  =>
               Use_Standard_Input := True;
               Output_Target := Standard_Output;

            when Unknown_Option =>
               return Parsed_CLI'(Status          => Unknown_Option,
                                  Name_Length     => Argument (Cursor)'Length,
                                  Password_Length => 0,
                                  Explanation     => Argument (Cursor));
            when Missing_Parameter =>
               return Parsed_CLI'(Status          => Missing_Parameter,
                                  Name_Length     => Argument (Cursor)'Length,
                                  Password_Length => 0,
                                  Explanation     => Argument (Cursor));

            when Unrequested_Parameter =>
               return Parsed_CLI'(Status          => Unrequested_Parameter,
                                  Name_Length     => Argument (Cursor)'Length,
                                  Password_Length => 0,
                                  Explanation     => Argument (Cursor));

            when Bad_Option_Syntax =>
               return Parsed_CLI'(Status          => Unknown_Option,
                                  Name_Length     => Argument (Cursor)'Length,
                                  Password_Length => 0,
                                  Explanation     => Argument (Cursor));

            when End_Of_Options =>
               null;
         end case;
      end loop;

      pragma Compile_Time_Warning
        (Standard.True, "Parse_Command_Line unimplemented");
      return
      raise Program_Error with "Unimplemented function Parse_Command_Line";
   end Parse_Command_Line;

   -----------
   -- Is_Ok --
   -----------

   function Is_Ok (Item : Parsed_CLI) return Boolean is
   begin
      pragma Compile_Time_Warning (Standard.True, "Is_Ok unimplemented");
      return raise Program_Error with "Unimplemented function Is_Ok";
   end Is_Ok;

   -------------------
   -- Error_Message --
   -------------------

   function Error_Message (Item : Parsed_CLI) return String is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Error_Message unimplemented");
      return raise Program_Error with "Unimplemented function Error_Message";
   end Error_Message;

   -------------
   -- Command --
   -------------

   function Command (Item : Parsed_CLI) return Command_Type is
   begin
      pragma Compile_Time_Warning (Standard.True, "Command unimplemented");
      return raise Program_Error with "Unimplemented function Command";
   end Command;

   ---------------
   -- Entry_Key --
   ---------------

   function Entry_Key (Item : Parsed_CLI) return String is
   begin
      pragma Compile_Time_Warning (Standard.True, "Entry_Key unimplemented");
      return raise Program_Error with "Unimplemented function Entry_Key";
   end Entry_Key;

   ---------------------------
   -- Use_Provided_Password --
   ---------------------------

   function Use_Provided_Password (Item : Parsed_CLI) return Boolean is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Use_Provided_Password unimplemented");
      return
      raise Program_Error
        with "Unimplemented function Use_Provided_Password";
   end Use_Provided_Password;

   -------------------
   -- User_Password --
   -------------------

   function User_Password (Item : Parsed_CLI) return String is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "User_Password unimplemented");
      return raise Program_Error with "Unimplemented function User_Password";
   end User_Password;

   -------------------
   -- Password_Spec --
   -------------------

   function Password_Spec
     (Item : Parsed_CLI) return Password_Conditions.Condition_Type
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Password_Spec unimplemented");
      return raise Program_Error with "Unimplemented function Password_Spec";
   end Password_Spec;

   ---------------------
   -- Password_Length --
   ---------------------

   function Password_Length (Item : Parsed_CLI) return Positive is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Password_Length unimplemented");
      return raise Program_Error with "Unimplemented function Password_Length";
   end Password_Length;

   ---------------------
   -- Password_Target --
   ---------------------

   function Password_Target (Item : Parsed_CLI) return Target_Name is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Password_Target unimplemented");
      return raise Program_Error with "Unimplemented function Password_Target";
   end Password_Target;

end Clortho.Command_Line;
