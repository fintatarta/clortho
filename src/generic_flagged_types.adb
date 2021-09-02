pragma Ada_2012;
package body Generic_Flagged_Types is
   pragma SPARK_Mode;

   ---------
   -- Set --
   ---------

   procedure Set (X : in out Flagged_Type; Val : Root_Type) is
   begin
      X := Flagged_Type'(Defined => True,
                         Value   => Val);

      pragma Assert (Value (X) = Val);
   end Set;
end Generic_Flagged_Types;
