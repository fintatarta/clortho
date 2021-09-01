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


   --   subtype Error is Option_Symbol range Unknown_Option .. Bad_Option_Syntax;





   function Is_Action_Set (Item : Option_Record) return Boolean
   is (Item.To_Do /= Get_Password);

   procedure Scan_Options (Cursor : out Positive;
                           Result : out Option_Record);



   ------------------------
   -- Parse_Command_Line --
   ------------------------

   function Parse_Command_Line return Parsed_CLI is


      function Double_Action return Parsed_CLI;
      function Double_Password return Parsed_CLI;
      function Double_Password_Length return Parsed_CLI;
      function Double_Specs return Parsed_CLI;

      function Bad_Integer (X : Unbounded_String) return Parsed_CLI;


      function Double_Action return Parsed_CLI
      is (Parsed_CLI'(Status => Double_Action));

      function Bad_Integer (X : Unbounded_String) return Parsed_CLI
      is (Parsed_CLI'(Status      => Bad_Integer,
                      Explanation => X));

      function Double_Password return Parsed_CLI
      is (Parsed_CLI'(Status => Double_Password));

      function Double_Specs return Parsed_CLI
      is (Parsed_CLI'(Status => Double_Specs));

      function Double_Password_Length return Parsed_CLI
      is (Parsed_CLI'(Status => Double_Password_Length));


      No_User_Password : constant Unbounded_String := Null_Unbounded_String;
      No_Password_Len  : constant Natural := 0;
      No_Specs         : constant Unbounded_String := Null_Unbounded_String;

      Cursor    : Positive;

   begin
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

         --  Back_Step := Target_Name'Pos (Output_Target);

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
