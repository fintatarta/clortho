with Ada.Strings.Maps;

use Ada;

package Clortho.Password_Conditions is
   use type Ada.Strings.Maps.Character_Set;

   type Condition_Type is private;

   function Create (Prohibited : Strings.Maps.Character_Set := Strings.Maps.Null_Set)
                    return Condition_Type
     with
       Post => Prohibited_Set (Create'Result) = Prohibited;

   function Allowed_Chars (Condition : Condition_Type)
                           return Strings.Maps.Character_Set;

   function Prohibited_Set (Condition : Condition_Type)
                            return Strings.Maps.Character_Set;

   type Repetition_Limit is private;

   subtype Repetition_Count is Integer range 0 .. Integer'Last - 1;

   function To_Limit (N : Repetition_Count) return Repetition_Limit;

   function "<=" (X, Y : Repetition_Limit) return Boolean;

   Infinity : constant Repetition_Limit;

   procedure Add_Condition (To  : in out Condition_Type;
                            Set : Strings.Maps.Character_Set;
                            Min : Repetition_Limit;
                            Max : Repetition_Limit)
     with
       Pre =>
         Min <= Max
         and To_Limit (1) <= Max
         and ((Set and Allowed_Chars (To)) = Strings.Maps.Null_Set),
         Post =>
           (Set and not Prohibited_Set (To)) = (Set and not Prohibited_Set (To) and Allowed_Chars (To));

   function Match (Item      : String;
                   Condition : Condition_Type)
                   return Boolean;

private
   type Repetition_Limit is range 0 .. Integer'Last;

   Infinity : constant Repetition_Limit := Repetition_Limit'Last;

   type Condition_Type is null record;
end Clortho.Password_Conditions;
