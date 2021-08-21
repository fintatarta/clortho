pragma Ada_2012;
with Ada.Strings.Maps.Constants;
with Clortho.Password_Style.Utilities;
with String_Scanners;

package body Clortho.Password_Style is
   pragma SPARK_Mode;

   procedure Parse_Simple_Spec (Input         : String;
                                Set           : out Ada.Strings.Maps.Character_Set;
                                Is_Prohibited : out Boolean)
     with
       Pre => Input'Length < Integer'Last;

   procedure Parse_Simple_Spec (Input         : String;
                                Set           : out Ada.Strings.Maps.Character_Set;
                                Is_Prohibited : out Boolean)
   is
      use Ada.Strings.Maps;
      use String_Scanners;

      --
      --  A simple spec is a concatenation of atomic specs.  An atomic spec can be
      --
      --  1. A three character sequence with a '-' in second position c '-' d
      --  2. A single character
      --
      --  More precisely, it satisfies the following regexp
      --
      --   !? ^? (char (- char)?)+
      --
      --  If ! is present the spec describes a prohibited set
      --  If ^ is present, the set must be complemented
      --

      Scanner : String_Scanners.Scanner_Type := String_Scanners.Create (Input);
      Complement_Result : Boolean;

      procedure Check_If_Prohibited (Scanner       : in out Scanner_Type;
                                     Is_Prohibited : out Boolean);

      procedure Check_If_Complement (Scanner    : in out Scanner_Type;
                                     Complement : out Boolean);

      procedure Check_If_Prohibited (Scanner       : in out Scanner_Type;
                                     Is_Prohibited : out Boolean)
      is
      begin
         if not End_Of_Input  (Scanner) and then Current_Char (Scanner) = '!' then
            Is_Prohibited := True;
            Next (Scanner);
         else
            Is_Prohibited := False;
         end if;
      end Check_If_Prohibited;

      procedure Check_If_Complement (Scanner    : in out Scanner_Type;
                                     Complement : out Boolean)
      is
      begin
         if not End_Of_Input (Scanner) and then Current_Char (Scanner) = '^' then
            Complement := True;
            Next (Scanner);
         else
            Complement := False;
         end if;
      end Check_If_Complement;
   begin
      Set := Ada.Strings.Maps.Null_Set;

      Check_If_Prohibited (Scanner, Is_Prohibited);
      Check_If_Complement (Scanner, Complement_Result);

      while not End_Of_Input (Scanner) loop
         if Remaining (Scanner) >= 3 and then Peek_Ahead (Scanner) = '-' then
            Set := Set or To_Set (Character_Range'(Low  => Current_Char (Scanner),
                                                   High => Peek_Ahead (Scanner, 2)));
            Next (Scanner, 3);
         else
            Set := Set or To_Set (Current_Char (Scanner));
            Next (Scanner);
         end if;

         pragma Loop_Variant (Decreases => Remaining (Scanner));
      end loop;

      if Complement_Result then
         Set := Constants.Graphic_Set - Set;
      end if;
   end Parse_Simple_Spec;

   -----------
   -- Parse --
   -----------

   function Parse (Input : String) return Password_Conditions.Condition_Type is
      use type Ada.Strings.Maps.Character_Set;
      use Utilities;

      Prohibited : Ada.Strings.Maps.Character_Set := Ada.Strings.Maps.Null_Set;
      Mandatory  : Set_Buffer;
      Scanner    : Segment_Scanner := Create (Input);

      Is_Prohibited : Boolean;
      Set           : Ada.Strings.Maps.Character_Set;
   begin
      while not End_Of_Input (Scanner) loop
         Parse_Simple_Spec (Current_Segment (Scanner), Set, Is_Prohibited);
         Next_Segment (Scanner);

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
