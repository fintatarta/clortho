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

   Bad_Limits : exception;
   Condition_Full : exception;
   Not_A_Partition : exception;

private
   type Repetition_Limit is range 0 .. Integer'Last;

   Infinity : constant Repetition_Limit := Repetition_Limit'Last;

   type Simple_Condition is
      record
         Set : Strings.Maps.Character_Set;
         Min : Repetition_Limit;
         Max : Repetition_Limit;
      end record;

   Bogus_Condition : constant Simple_Condition := (Set => Strings.Maps.Null_Set,
                                                   Min => Infinity,
                                                   Max => Infinity);

   type Extended_Condition_ID is range 0 .. 256;
   subtype Simple_Condition_ID is Extended_Condition_ID range 1 .. 256;

   No_Id : constant Extended_Condition_ID := Extended_Condition_ID'First;

   pragma Compile_Time_Error (No_Id in Simple_Condition_ID, "Bad No_ID value");

   type Simple_Condition_Array is
     array (Simple_Condition_ID) of Simple_Condition;

   function Is_Valid (Prob       : Strings.Maps.Character_Set;
                      Elementary : Simple_Condition_Array;
                      Free       : Simple_Condition_ID)
                      return Boolean;

   type Condition_Type is
      record
         Prohibited : Strings.Maps.Character_Set;

         Elementary_Conditions : Simple_Condition_Array := (others =>  Bogus_Condition);

         First_Free : Simple_Condition_ID := Simple_Condition_ID'First;
      end record
     with
       Dynamic_Predicate => Is_Valid (Prob       => Condition_Type.Prohibited,
                                      Elementary => Condition_Type.Elementary_Conditions,
                                      Free       => Condition_Type.First_Free);
end Clortho.Password_Conditions;
