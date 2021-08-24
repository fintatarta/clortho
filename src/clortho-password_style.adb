pragma Ada_2012;
with Ada.Strings.Maps;

with Password_Style_Parsers;

package body Clortho.Password_Style is
   pragma SPARK_Mode;

   -----------
   -- Parse --
   -----------

   -----------
   -- Parse --
   -----------

   function Parse (Input       : String;
                   Missing_Are : Missing_Option)
                   return Password_Conditions.Condition_Type
   is
      use type Ada.Strings.Maps.Character_Set;
      --  use Utilities;
      use Password_Style_Parsers;

      Prohibited_Chars : Ada.Strings.Maps.Character_Set := Ada.Strings.Maps.Null_Set;
      Optional_Chars   : Ada.Strings.Maps.Character_Set := Ada.Strings.Maps.Null_Set;

      Sets          : constant Password_Style_Descriptor := Parse (Input);
   begin
      if not Is_Valid (Sets) then
         raise Overlapping_Sets;
      end if;

      Prohibited_Chars := Sets.Prohibited;

      case Missing_Are is
         when  Prohibited =>
            Prohibited_Chars := Prohibited_Chars or Missing (Sets);

         when Optional =>
            Optional_Chars := Missing (Sets);
      end case;

      declare
         use Password_Conditions;
         Result : Condition_Type := Create (Prohibited => Prohibited_Chars);
      begin
         for Set of Sets.Mandatory loop
            Add_Mandatory (Result, Set);
         end loop;

         if Optional_Chars /= Ada.Strings.Maps.Null_Set then
            Add_Optional (Result, Optional_Chars);
         end if;

         return Result;
      end;
   end Parse;

end Clortho.Password_Style;
