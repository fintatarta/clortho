with Clortho.Password_Conditions;

package Clortho.Password_Style with SPARK_Mode is
   type Missing_Option is (Prohibited, Optional);

   type Exit_Status is (Ok, Parsing_Error, Overlapping_Sets);

   type Parsing_Result (Status : Exit_Status) is
      record
         case Status is
            when Ok =>
               Conditions : Password_Conditions.Condition_Type;

            when others =>
               null;
         end case;
      end record;

   function Parse (Input       : String;
                   Missing_Are : Missing_Option)
                   return Parsing_Result
     with
       Pre =>
         Input'Length < Integer'Last
         and Input'Length >= 3;
end Clortho.Password_Style;
