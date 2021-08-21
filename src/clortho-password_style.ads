with Clortho.Password_Conditions;

package Clortho.Password_Style with SPARK_Mode is
   function Parse (Input : String) return Password_Conditions.Condition_Type
     with
       Pre =>
         Input'Length < Integer'Last
         and Input'Length >= 3;

   Parsing_Error : exception;
end Clortho.Password_Style;
