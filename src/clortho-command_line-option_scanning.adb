pragma Ada_2012;
with Ada.Strings.Fixed;

package body Clortho.Command_Line.Option_Scanning is
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
      List,
      End_Of_Options
      --  Unknown_Option,
      --  Missing_Parameter,
      --  Unrequested_Parameter,
      --  Bad_Option_Syntax
     );

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
                       List                  => False,
                       End_Of_Options        => False
                       --  Unknown_Option        => False,
                       --  Missing_Parameter     => False,
                       --  Unrequested_Parameter => False,
                       --  Bad_Option_Syntax     => False
                      );

   type Option_Name_Pair is
      record
         Name           : Unbounded_String;
         Symbol         : Option_Symbol;
      end record;

   type Descriptor_Array is array (Positive range <>) of Option_Name_Pair;

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
                (+"inout", Filter),
                (+"list", List)
                --  (+"not-smart", Non_Url_Matching),
                --  (+"strict", Non_Url_Matching)
               );

   procedure Start_Option_Scanning (Cursor : out Positive);

   procedure Next_Option (Cursor    : in out Positive;
                          Option    :    out Option_Symbol;
                          Parameter :    out Unbounded_String;
                          Success   :    out Error_Status)
     with
       Pre =>
         Cursor <= Argument_Count + 1,
         Post =>
           (if Success = Ok then
              (if not Need_Parameter (Option)
                 then Parameter = Null_Unbounded_String)
            and (if Cursor'Old = Argument_Count + 1
                        then Cursor = Cursor'Old and Option = End_Of_Options));

   procedure Parse_Single_Option (Input     : String;
                                  Option    : out Option_Symbol;
                                  Parameter : out Unbounded_String;
                                  Success   : out Error_Status);

   procedure Parse_Integer (Input : String;
                            Value : out Positive;
                            OK    : out Boolean);

   procedure Start_Option_Scanning (Cursor : out Positive)
   is
   begin
      Cursor := 1;
   end Start_Option_Scanning;

   procedure Next_Option (Cursor    : in out Positive;
                          Option    :    out Option_Symbol;
                          Parameter :    out Unbounded_String;
                          Success   :    out Error_Status)

   is
   begin
      if Cursor = Argument_Count + 1 then
         Option := End_Of_Options;
         Parameter := Null_Unbounded_String;
         Success := Ok;
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
            Success := Ok;
            return;
         end if;

         if
           Arg'Length < 3
           or else Arg (Arg'First .. Arg'First + 1) /= "--"
         then
            Option := End_Of_Options;
            Parameter := Null_Unbounded_String;
            Success := Ok;
            return;
         end if;

         pragma Assert (Arg'Length >= 3
                        and Arg (Arg'First .. Arg'First + 1) = "--");

         Parse_Single_Option (Arg (Arg'First + 2 .. Arg'Last),
                              Option,
                              Parameter,
                              Success);
      end;
   end Next_Option;

   procedure Parse_Single_Option (Input     : String;
                                  Option    : out Option_Symbol;
                                  Parameter : out Unbounded_String;
                                  Success   : out Error_Status)
   is
      use Ada.Strings.Fixed;

      procedure To_Symbol (X       : String;
                           Symbol  : out Option_Symbol;
                           Success : out Error_Status);

      procedure To_Symbol (X       : String;
                           Symbol  : out Option_Symbol;
                           Success : out Error_Status)
      is
      begin
         for E of Options loop
            if E.Name = To_Unbounded_String (X) then
               Symbol := E.Symbol;
               Success := Ok;
               return;
            end if;
         end loop;

         Symbol := Option_Symbol'First;
         Success := Unknown_Option;
      end To_Symbol;

      Equal_Position : constant Natural := Index (Source  => Input,
                                                  Pattern => "=");
   begin
      if Equal_Position = 0 then
         Parameter := Null_Unbounded_String;
         To_Symbol (Input, Option, Success);

         if Success /= Ok then
            return;
         end if;

         if Need_Parameter (Option) then
            Success := Missing_Parameter;
            return;
         end if;

      elsif
        Equal_Position = Input'First or Equal_Position = Input'Last
      then
         Success := Bad_Option_Syntax;
         Parameter := Null_Unbounded_String;
         return;

      else
         declare
            Name : constant String :=
                     Input (Input'First .. Equal_Position - 1);

            Value : constant String :=
                      Input (Equal_Position + 1 .. Input'Last);
         begin
            Parameter := To_Unbounded_String (Value);
            To_Symbol (Name, Option, Success);

            if Success /= Ok then
               return;
            end if;

            if not Need_Parameter (Option) then
               Success := Unrequested_Parameter;
               Parameter := Null_Unbounded_String;
            else
               Parameter := To_Unbounded_String (Value);
            end if;
         end;
      end if;

      Success := Ok;
   end Parse_Single_Option;

   procedure Parse_Integer (Input : String;
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
   end Parse_Integer;

   ------------------
   -- Scan_Options --
   ------------------

   procedure Scan_Options (Cursor :    out Positive;
                           Result : in out Option_Sets.Option_Set;
                           Err    :    out Option_Processing_Error)
   is
      use Option_Sets;

      Option    : Option_Symbol;
      Parameter : Unbounded_String;
   begin
      Start_Option_Scanning (Cursor);

      loop
         Next_Option (Cursor, Option, Parameter, Err);

         exit when Option = End_Of_Options or Err /= Ok;

         case Option is
            when Old =>
               if Option_Sets.Is_Defined (Action (Result)) then
                  Err := Double_Action;
                  return;
               end if;

               Option_Sets.Do_Get_Password (Result, 1);

            when Back =>
               if Option_Sets.Is_Defined (Action (Result)) then
                  Err := Double_Action;
                  return;
               end if;

               declare
                  OK : Boolean;
                  N  : Positive;
               begin
                  Parse_Integer (To_String (Parameter), N, OK);

                  if not OK then
                     Err := Bad_Integer;
                     return;
                  end if;

                  Option_Sets.Do_Get_Password (Result, N);
               end;

            when Create =>
               if Option_Sets.Is_Defined (Action (Result)) then
                  Err := Double_Action;
                  return;
               end if;

               Option_Sets.Do_Create_Entry (Result);

            when Renew =>
               if Option_Sets.Is_Defined (Action (Result)) then
                  Err := Double_Action;
                  return;
               end if;

               Option_Sets.Do_Renew_Password (Result);

            when Vacuum =>
               if Option_Sets.Is_Defined (Action (Result)) then
                  Err := Double_Action;
                  return;
               end if;

               Option_Sets.Do_Vacuum (Result);

            when Full_Vacuum =>
               if Option_Sets.Is_Defined (Action (Result)) then
                  Err := Double_Action;
                  return;
               end if;

               Option_Sets.Do_Full_Vacuum (Result);

            when Delete =>
               if Option_Sets.Is_Defined (Action (Result)) then
                  Err := Double_Action;
                  return;
               end if;

               Option_Sets.Do_Delete (Result);

            when Roll_Back =>
               if Option_Sets.Is_Defined (Action (Result)) then
                  Err := Double_Action;
                  return;
               end if;

               Option_Sets.Do_Roll_Back (Result);

            when List =>
               if Option_Sets.Is_Defined (Action (Result)) then
                  Err := Double_Action;
                  return;
               end if;

               Option_Sets.Do_List (Result);

            when  User_Password =>
               if Is_Defined (User_Password (Result)) then
                  Err := Double_Password;
                  return;
               end if;

               Option_Sets.Set_User_Password (Result, Parameter);

            when Password_Length =>
               if Option_Sets.Is_Password_Length_Specified (Result) then
                  Err := Double_Password_Length;
                  return;
               end if;

               declare
                  OK : Boolean;
                  N  : Positive;
               begin
                  Parse_Integer (To_String (Parameter), N, OK);

                  if not OK  then
                     Err := Bad_Integer;
                     return;
                  end if;

                  Option_Sets.Set_Password_Nchars (Result, N);
               end;

            when Password_Bits =>
               if Is_Defined (Password_Spec (Result)) then
                  Err := Double_Password_Length;
                  return;
               end if;

               declare
                  OK : Boolean;
                  N  : Positive;
               begin
                  Parse_Integer (To_String (Parameter), N, OK);

                  if not OK  then
                     Err := Bad_Integer;
                     return;
                  end if;

                  Option_Sets.Set_Password_Nbits (Result, N);
               end;

            when Password_Spec =>
               if Is_Defined (Password_Spec (Result)) then
                  Err := Double_Specs;
                  return;
               end if;

               Option_Sets.Set_Password_Specs (Result, Parameter);

            when Input =>
               Option_Sets.Use_Source (Result, Standard_Input);

            when Output =>
               Option_Sets.Use_Target (Result, Standard_Output);

            when Filter  =>
               Option_Sets.Use_Source (Result, Standard_Input);
               Option_Sets.Use_Target (Result, Standard_Output);

               --  when Unknown_Option =>
               --     return Parsed_CLI'(Status          => Unknown_Option,
               --                        Explanation     => +Argument (Cursor));
               --
               --  when Missing_Parameter =>
               --     return Parsed_CLI'(Status          => Missing_Parameter,
               --                        Explanation     => +Argument (Cursor));
               --
               --  when Unrequested_Parameter =>
               --     return Parsed_CLI'(Status          => Unrequested_Parameter,
               --                        Explanation     => +Argument (Cursor));
               --
               --  when Bad_Option_Syntax =>
               --     return Parsed_CLI'(Status          => Unknown_Option,
               --                        Explanation     => +Argument (Cursor));

            when End_Of_Options =>
               null;
         end case;
      end loop;

      if not Is_Defined (Action (Result)) then
         Do_Get_Password (Result, 0);
      end if;
   end Scan_Options;

end Clortho.Command_Line.Option_Scanning;
