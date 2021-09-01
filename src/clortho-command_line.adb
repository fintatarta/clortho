pragma Ada_2012;
with Ada.Command_Line;
with Ada.Strings.Fixed;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO;

with Clortho.Clipboard;

--  with Password_Style_Parsers;

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
      function Argument_Count return Natural
        with
          Global => null;

      function Argument (N : Positive) return String
        with
          Global => null;

      procedure Get_Line (Item : out Unbounded_String)
        with
          Global => null;

      function Argument_Count return Natural
      is (Ada.Command_Line.Argument_Count)
        with  SPARK_Mode => Off;

      function Argument (N : Positive) return String
      is (Ada.Command_Line.Argument (N))
        with SPARK_Mode => Off;

      procedure Get_Line (Item : out Unbounded_String)
        with SPARK_Mode => Off
      is
      begin
         Unbounded_IO.Get_Line (Item);
      end Get_Line;

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
                      Name_Length     => 0));

      function Bad_Integer (X : Unbounded_String) return Parsed_CLI
      is (Parsed_CLI'(Status          => Bad_Integer,
                      Name_Length     => 0,
                      Explanation     => X));

      function Double_Password return Parsed_CLI
      is (Parsed_CLI'(Status          => Double_Password,
                      Name_Length     => 0));

      function Double_Specs return Parsed_CLI
      is (Parsed_CLI'(Status          => Double_Specs,
                      Name_Length     => 0));

      function Double_Password_Length return Parsed_CLI
      is (Parsed_CLI'(Status          => Double_Password_Length,
                      Name_Length     => 0));

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
      No_Password_Len  : constant Natural := 0;
      No_Specs         : constant Unbounded_String := Null_Unbounded_String;

      Cursor    : Positive;
      Option    : Option_Symbol;
      Parameter : Unbounded_String;

      To_Do                  : Command_Type := Get_Password;
      Back_Step              : Natural := 0;
      Password_Len           : Natural := No_Password_Len;
      Password_N_Bits        : Natural := No_Password_Len;
      User_Provided_Password : Unbounded_String := No_User_Password;
      Output_Target          : Target_Name := Clipboard;
      Specs                  : Unbounded_String := No_Specs;
      Use_Standard_Input     : Boolean := False;
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
                                  Name_Length     => 0,
                                  Explanation     => +Argument (Cursor));

            when Missing_Parameter =>
               return Parsed_CLI'(Status          => Missing_Parameter,
                                  Name_Length     => 0,
                                  Explanation     => +Argument (Cursor));

            when Unrequested_Parameter =>
               return Parsed_CLI'(Status          => Unrequested_Parameter,
                                  Name_Length     => 0,
                                  Explanation     => +Argument (Cursor));

            when Bad_Option_Syntax =>
               return Parsed_CLI'(Status          => Unknown_Option,
                                  Name_Length     => 0,
                                  Explanation     => +Argument (Cursor));

            when End_Of_Options =>
               null;
         end case;
      end loop;

      declare
         type Result_Type is (Name_Found, No_Name_Found, Name_Error);

         type Foo (Error  : Result_Type) is
            record
               case Error is
                  when Name_Found =>
                     Name : Unbounded_String;

                  when No_Name_Found | Name_Error =>
                     null;
               end case;
            end record;

         function Get_Name (Cursor               : Positive;
                            On_Command_Line_Only : Boolean;
                            Use_Standard_Input   : Boolean)
                            return Foo;

         function Get_Name (Cursor               : Positive;
                            On_Command_Line_Only : Boolean;
                            Use_Standard_Input   : Boolean)
                            return Foo
         is
         begin
            if Cursor < Argument_Count then
               return Foo'(Error => Name_Error);

            elsif Cursor = Argument_Count then
               return Foo'(Error  => Name_Found,
                           Name   => +Argument (Cursor));

            elsif On_Command_Line_Only then
               return Foo'(Error => No_Name_Found);

            else
               declare
                  Name : Unbounded_String;
               begin
                  if Use_Standard_Input then
                     Get_Line (Name);
                  else
                     Clortho.Clipboard.Get_Clipboard (Name);
                  end if;

                  return Foo'(Error  => Name_Found,
                              Name   => Name);
               end;
            end if;

         end Get_Name;

         Name : constant Foo :=
                  Get_Name (Cursor               => Cursor,
                            On_Command_Line_Only => To_Do /= Get_Password,
                            Use_Standard_Input   => Use_Standard_Input);
      begin

         if Name.Error = Name_Error then
            raise Constraint_Error;
         end if;

         Back_Step := Target_Name'Pos (Output_Target);

         case To_Do is
            when Get_Password =>
               null;
            when Get_Old_Password =>
               null;
            when Create_Entry =>
               null;
            when Renew_Password =>
               null;
            when Vacuum_Entry =>
               null;
            when Roll_Back_Entry =>
               null;
            when Delete_Entry =>
               null;
            when Vacuum_All =>
               null;
         end case;
      end;
      pragma Compile_Time_Warning
        (Standard.False, "Parse_Command_Line unimplemented");
      return
      raise Program_Error with "Unimplemented function Parse_Command_Line";
   end Parse_Command_Line;

   -----------
   -- Is_Ok --
   -----------

   function Is_Ok (Item : Parsed_CLI) return Boolean
   is (Item.Status = Ok);

   -------------------
   -- Error_Message --
   -------------------

   function Explanation (Item : Parsed_CLI) return String
   is ("'" & To_String (Item.Explanation) & "'")
     with Pre => Item.Status in Error_With_Explanation;

   function Error_Message (Item : Parsed_CLI) return String
   is (case Item.Status is
          when Ok                     =>
             "Ok",

          when Unknown_Option         =>
             "Unknown option " & Explanation (Item),

          when Missing_Parameter      =>
             "Missing option parameter",

          when Unrequested_Parameter  =>
             "Unexpected parameter " & Explanation (Item),

          when Bad_Option_Syntax      =>
             "Bad syntax in option " & Explanation (Item),

          when Double_Action          =>
             "More than one action specified",

          when Bad_Integer            =>
             "Bad integer " & Explanation (Item),

          when Double_Password        =>
             "Secret specified more than once",

          when Double_Password_Length =>
             "Secret length specified more than once",

          when Double_Specs           =>
             "Secret specs specified more than once"
      );

   -------------
   -- Command --
   -------------

   function Command (Item : Parsed_CLI) return Command_Type
   is (Item.Command);

   ---------------
   -- Entry_Key --
   ---------------

   function Entry_Key (Item : Parsed_CLI) return String
   is (Item.Name);

   ---------------------------
   -- Use_Provided_Password --
   ---------------------------

   function Use_Provided_Password (Item : Parsed_CLI) return Boolean
   is (Item.User_Password /= Null_Unbounded_String);

   -------------------
   -- User_Password --
   -------------------

   function User_Password (Item : Parsed_CLI) return String
   is (To_String (Item.User_Password));

   -------------------
   -- Password_Spec --
   -------------------

   function Password_Spec
     (Item : Parsed_CLI) return Password_Conditions.Condition_Type
   is (Item.Specs);

   ---------------------
   -- Password_Length --
   ---------------------

   function Password_Length (Item : Parsed_CLI) return Positive
   is (Item.Password_Length);

   ---------------------
   -- Password_Target --
   ---------------------

   function Password_Target (Item : Parsed_CLI) return Target_Name
   is (Item.Target);

end Clortho.Command_Line;
