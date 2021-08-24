with Clortho.Password_Conditions;

package Clortho.Password_Style with SPARK_Mode is
   type Missing_Option is (Prohibited, Optional);

   function Parse (Input       : String;
                   Missing_Are : Missing_Option)
                   return Password_Conditions.Condition_Type
     with
       Pre =>
         Input'Length < Integer'Last
         and Input'Length >= 3;

   Parsing_Error : exception;
   Overlapping_Sets : exception;
end Clortho.Password_Style;
