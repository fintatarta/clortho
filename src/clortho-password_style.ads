with Clortho.Password_Conditions;

package Clortho.Password_Style is
   function Parse (Input : String) return Password_Conditions.Condition_Type;

   Parsing_Error : exception;
end Clortho.Password_Style;
