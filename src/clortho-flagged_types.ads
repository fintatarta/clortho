with Ada.Strings.Unbounded;       use Ada.Strings.Unbounded;

with Generic_Flagged_Types;

package Clortho.Flagged_Types is
   pragma SPARK_Mode;

   package Flagged_Naturals is
     new Generic_Flagged_Types (Root_Type     => Natural,
                                Default_Value => 0);

   subtype Flagged_Natural is Flagged_Naturals.Flagged_Type;

   package Flagged_Positives is
     new Generic_Flagged_Types (Root_Type     => Positive,
                                Default_Value => 1);

   subtype Flagged_Positive is Flagged_Positives.Flagged_Type;

   package Flagged_Strings is
     new Generic_Flagged_Types (Root_Type     => Unbounded_String,
                                Default_Value => Null_Unbounded_String);

   subtype Flagged_String is Flagged_Strings.Flagged_Type;
end Clortho.Flagged_Types;
