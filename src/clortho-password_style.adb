pragma Ada_2012;
with Ada.Strings.Maps.Constants;
with Clortho.Password_Style.Utilities;
with String_Scanners;

package body Clortho.Password_Style is

   procedure Parse_Simple_Spec (Input         : String;
                                Set           : out Ada.Strings.Maps.Character_Set;
                                Is_Prohibited : out Boolean);

   procedure Parse_Simple_Spec (Input         : String;
                                Set           : out Ada.Strings.Maps.Character_Set;
                                Is_Prohibited : out Boolean)
   is
      use Ada.Strings.Maps;

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

      procedure Check_If_Prohibited;
      procedure Check_If_Complement;

      procedure Check_If_Prohibited
      is
      begin
         if Scanner.Current_Char = '!' then
            Is_Prohibited := True;
            Scanner.Next;
         else
            Is_Prohibited := False;
         end if;
      end Check_If_Prohibited;

      procedure Check_If_Complement
      is
      begin
         if Scanner.Current_Char = '^' then
            Complement_Result := True;
            Scanner.Next;
         else
            Complement_Result := False;
         end if;
      end Check_If_Complement;
   begin
      Set := Ada.Strings.Maps.Null_Set;

      Check_If_Prohibited;
      Check_If_Complement;

      while not Scanner.End_Of_Input loop
         if Scanner.Remaining >= 3 and then Scanner.Peek_Ahead = '-' then
            Set := Set or To_Set (Character_Range'(Low  => Scanner.Current_Char,
                                                   High => Scanner.Peek_Ahead (2)));
            Scanner.Next (3);
         else
            Set := Set or To_Set (Scanner.Current_Char);
            Scanner.Next;
         end if;

         pragma Loop_Variant (Decreases => Scanner.Remaining);
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

      Prohibited : Ada.Strings.Maps.Character_Set;
      Mandatory  : Set_Buffer (Input'Length);
      Scanner    : Input_Scanner := Create (Input);

      Is_Prohibited : Boolean;
      Set           : Ada.Strings.Maps.Character_Set;
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
