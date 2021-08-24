with Clortho.Password_Conditions;

package Clortho.Password_Generation is
   pragma SPARK_Mode;

   function Get_Password (Length     : Positive;
                          Constraint : Password_Conditions.Condition_Type)
                          return String
     with
       Post =>
         Get_Password'Result'Length = Length
         and Password_Conditions.Match (Get_Password'Result, Constraint);
end Clortho.Password_Generation;
