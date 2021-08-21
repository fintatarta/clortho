pragma Ada_2012;
with Ada.Strings.Maps.Constants;
with Clortho.Password_Style.Utilities;

package body Clortho.Password_Style is

   procedure Parse_Simple_Spec (Input         : String;
                                Set           : out Ada.Strings.Maps.Character_Set;
                                Is_Prohibited : out Boolean);

   procedure Parse_Simple_Spec (Input         : String;
                                Set           : out Ada.Strings.Maps.Character_Set;
                                Is_Prohibited : out Boolean)
   is
   begin
      pragma Compile_Time_Warning (True, "Parse_Simple_Spec unimplemented");
      raise Program_Error;
   end Parse_Simple_Spec;

   -----------
   -- Parse --
   -----------

   function Parse (Input : String) return Password_Conditions.Condition_Type is
      use type Ada.Strings.Maps.Character_Set;
      use Utilities;

      Prohibited : Ada.Strings.Maps.Character_Set;
      Mandatory : Set_Buffer (Input'Length);
      Scanner : Input_Scanner := Create (Input);

      Is_Prohibited : Boolean;
      Set : Ada.Strings.Maps.Character_Set;
   begin
      while not End_Of_Input (Scanner) loop
         Parse_Simple_Spec (Next_Segment (Scanner), Set, Is_Prohibited);

         if Is_Prohibited then
            Prohibited := Prohibited or Set;

         else
            Append (To   => Mandatory,
                    Item => Set);
         end if;
      end loop;

      declare
         use Password_Conditions;
         use Ada.Strings.Maps;

         Result : Condition_Type := Create (Prohibited);
      begin
         Loop_Over (Mandatory);

         while Has_Element (Mandatory) loop
            Add_Condition (To  => Result,
                           Set => Current_Element (Mandatory),
                           Min => To_Limit (1),
                           Max => Infinity);

            Next (Mandatory);
         end loop;

         declare
            Optional : constant Character_Set :=
                         Constants.Graphic_Set
                             and not (Allowed_Chars (Result) or Prohibited_Set (Result));
         begin
            Add_Condition (To  => Result,
                           Set => Optional,
                           Min => To_Limit (0),
                           Max => Infinity);
         end;

         return Result;
      end;

   end Parse;

end Clortho.Password_Style;
