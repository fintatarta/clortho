generic
   type Root_Type is private;

   Default_Value : Root_Type;
package Generic_Flagged_Types is
   pragma SPARK_Mode;

   type Flagged_Type is private;

   function Undefined return Flagged_Type
     with
       Post => not Is_Defined (Undefined'Result);

   function Is_Defined (X : Flagged_Type) return Boolean;

   procedure Set (X    : in out Flagged_Type;
                  Val  : Root_Type)
     with
       Pre =>
         not Is_Defined (X),
       Post =>
         Is_Defined (X)
         and then Value (X) = Val;

   function Value (X : Flagged_Type) return Root_Type;
private
   type Flagged_Type is
      record
         Defined : Boolean := False;
         Value   : Root_Type := Default_Value;
      end record;

   function Undefined return Flagged_Type
   is (Flagged_Type'(False, Default_Value));

   function Is_Defined (X : Flagged_Type) return Boolean
   is (X.Defined);

   function Value (X : Flagged_Type) return Root_Type
   is (X.Value);
end Generic_Flagged_Types;
