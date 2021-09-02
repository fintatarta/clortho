pragma Ada_2012;
with Ada.Command_Line;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO;

with Clortho.Clipboard;
with Clortho.Command_Line.Option_Sets;
with Clortho.Command_Line.Option_Scanning;

--  with Password_Style_Parsers;

package body Clortho.Command_Line is
   pragma SPARK_Mode;

   function "+" (X : String) return Unbounded_String
                 renames To_Unbounded_String;

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

   subtype True_Error is
     Error_Status range Error_Status'Succ (Ok) .. Error_Status'Last;

   --
   --  Convert the error status into a Parsed_CLI record that can be
   --  returned to the caller of Parse_Comand_Line
   --
   function To_Parsed_CLI (Status : True_Error;
                           Cursor : Positive) return Parsed_CLI;

   type Result_Type is (Name_Found, No_Name_Found, Name_Error);

   --
   --  Read the "name," that is, the key used to access the DB of secrets
   --  Depending on the specific command, the name can be found in few
   --  different places
   --
   --  * First the command line is checked.  The name, if present,
   --    must be the last parameter
   --
   --  * For some commands (currently only Get_Password) the name can be
   --    obtained also from the clipboard (the default) or from the standard
   --    input (if the user specified the `--in` option)
   --
   --  The information about if a name was actually found or not (or any error
   --  occured) is returned into Result.
   --
   procedure Get_Name (Cursor               : Positive;
                       On_Command_Line_Only : Boolean;
                       Use_Standard_Input   : Boolean;
                       Name                 : out Unbounded_String;
                       Result               : out Result_Type);

   procedure Get_Name (Cursor               : Positive;
                       On_Command_Line_Only : Boolean;
                       Use_Standard_Input   : Boolean;
                       Name                 : out Unbounded_String;
                       Result               : out Result_Type)
   is
   begin
      if Cursor < Argument_Count then
         Name := Null_Unbounded_String;
         Result := Name_Error;
         return;

      elsif Cursor = Argument_Count then
         Name := +Argument (Cursor);
         Result := Name_Found;
         return;

      elsif On_Command_Line_Only then
         Name := Null_Unbounded_String;
         Result := No_Name_Found;
         return;

      else
         if Use_Standard_Input then
            Get_Line (Name);
         else
            Clortho.Clipboard.Get_Clipboard (Name);
         end if;

         Result := Name_Found;
         return;
      end if;
   end Get_Name;

   function To_Parsed_CLI (Status : True_Error;
                           Cursor : Positive) return Parsed_CLI
   is
      function Double_Action return Parsed_CLI;
      function Double_Password return Parsed_CLI;
      function Double_Password_Length return Parsed_CLI;
      function Double_Specs return Parsed_CLI;
      function Missing_Key return Parsed_CLI;
      function Unexpected_Key return Parsed_CLI;

      function Bad_Integer (X : String) return Parsed_CLI;
      function Unknown_Option (X : String) return Parsed_CLI;
      function Missing_Parameter (X : String) return Parsed_CLI;
      function Unrequested_Parameter (X : String) return Parsed_CLI;
      function Bad_Option_Syntax (X : String) return Parsed_CLI;

      function Double_Action return Parsed_CLI
      is (Parsed_CLI'(Status => Double_Action));

      function Double_Password return Parsed_CLI
      is (Parsed_CLI'(Status => Double_Password));

      function Double_Specs return Parsed_CLI
      is (Parsed_CLI'(Status => Double_Specs));

      function Double_Password_Length return Parsed_CLI
      is (Parsed_CLI'(Status => Double_Password_Length));

      function Missing_Key return Parsed_CLI
      is (Parsed_CLI'(Status => Missing_Key));

      function Unexpected_Key return Parsed_CLI
      is (Parsed_CLI'(Status => Unexpected_Key));

      function Bad_Integer (X : String) return Parsed_CLI
      is (Parsed_CLI'(Status      => Bad_Integer,
                      Explanation => To_Unbounded_String (X)));

      function Unknown_Option (X : String) return Parsed_CLI
      is (Parsed_CLI'(Status      => Unknown_Option,
                      Explanation => To_Unbounded_String (X)));

      function Missing_Parameter (X : String) return Parsed_CLI
      is (Parsed_CLI'(Status      => Missing_Parameter,
                      Explanation => To_Unbounded_String (X)));

      function Unrequested_Parameter (X : String) return Parsed_CLI
      is (Parsed_CLI'(Status      => Unrequested_Parameter,
                      Explanation => To_Unbounded_String (X)));

      function Bad_Option_Syntax (X : String) return Parsed_CLI
      is (Parsed_CLI'(Status      => Bad_Option_Syntax,
                      Explanation => To_Unbounded_String (X)));
   begin
      case Status is
         when Unknown_Option =>
            return Unknown_Option (Argument (Cursor));

         when Missing_Parameter =>
            return Missing_Parameter (Argument (Cursor));

         when Unrequested_Parameter =>
            return Unrequested_Parameter (Argument (Cursor));

         when Bad_Option_Syntax =>
            return Bad_Option_Syntax (Argument (Cursor));

         when Bad_Integer =>
            return Bad_Integer (Argument (Cursor));

         when Double_Action =>
            return Double_Action;

         when Double_Password =>
            return Double_Password;

         when Double_Password_Length =>
            return Double_Password_Length;

         when Double_Specs =>
            return Double_Specs;

         when Key_Processing_Error =>
            raise Program_Error;
      end case;
   end To_Parsed_CLI;

   ------------------------
   -- Parse_Command_Line --
   ------------------------

   function Parse_Command_Line return Parsed_CLI
   is
      Cursor  : Positive;
      Options : Option_Sets.Option_Set;
      Success : Error_Status;
   begin
      Option_Scanning.Scan_Options (Cursor => Cursor,
                                    Result => Options,
                                    Err    => Success);

      if Success /= Ok then
         return To_Parsed_CLI (Success, Cursor);
      end if;

      pragma Assert (Success = Ok);

      declare
         use Option_Sets;

         Name : Unbounded_String;
         Result : Result_Type;

      begin
         Get_Name (Cursor               => Cursor,
                   On_Command_Line_Only => Action (Options) /= Get_Password,
                   Use_Standard_Input   => Source (Options) = Standard_Input,
                   Name                 => Name,
                   Result               => Result);

         if Result = Name_Error then
            return To_Parsed_CLI (Bad_Command_Line, Cursor);
         end if;

         case Action (Options) is
            when Command_With_Parameter =>
               if Result = No_Name_Found then
                  return To_Parsed_CLI (Missing_Key, Cursor);
               end if;

            when others =>
               if Result = Name_Found then
                  return To_Parsed_CLI (Unexpected_Key, Cursor);
               end if;
         end case;

         case Action (Options)  is
            when Get_Password =>
               if
                 Is_Password_Provided (Options)
                 or Is_Password_Length_Specified (Options)
                 or Is_Password_Spec_Defined (Options)
               then
                  return To_Parsed_CLI (Bad_Command_Line, Cursor);
               end if;

               return Parsed_CLI'(Status          => Ok,
                                  Name            => Name,
                                  User_Password   => Null_Unbounded_String,
                                  Command         => Get_Password,
                                  Target          => Target (Options),
                                  Specs           => <>,
                                  Version         => Password_Version (Options),
                                  Password_Length => 1);

            when Create_Entry     =>
               null;
            when Renew_Password   =>
               null;
            when Vacuum_Entry     =>
               null;
            when Roll_Back_Entry  =>
               null;
            when Delete_Entry     =>
               null;
            when Vacuum_All       =>
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
             "Secret specs specified more than once",

          when Missing_Key            =>
             "This command requires a key",

          when Unexpected_Key         =>
             "This command does not require a key",

          when Bad_Command_Line       =>
             "Bad command line syntax"
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
   is (To_String (Item.Name));

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
