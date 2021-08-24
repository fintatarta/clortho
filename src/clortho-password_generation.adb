pragma Ada_2012;

with Ada.Strings.Maps;
with Clortho.IID_Drawing;

package body Clortho.Password_Generation is
   pragma SPARK_Mode;

   ------------------
   -- Get_Password --
   ------------------

   function Get_Password (Length     : Positive;
                          Constraint : Password_Conditions.Condition_Type)
                          return String
   is
      use Ada.Strings.Maps;
      use Password_Conditions;

      Result : String (1 .. Length);
      Valid_Charset : constant String := To_Sequence (Allowed_Chars (Constraint));
      Buffer : IID_Drawing.Random_Data (Result'Range);
   begin
      loop
         IID_Drawing.Fill (Data => Buffer,
                           Max  => Valid_Charset'Last,
                           Min  => Valid_Charset'First);

         for I in Result'Range loop
            Result (I) := Valid_Charset (Buffer (I));
         end loop;

         exit when Password_Conditions.Match (Result, Constraint);
      end loop;

      return Result;
   end Get_Password;

end Clortho.Password_Generation;
