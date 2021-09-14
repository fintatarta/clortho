package Hidden_Input is
   pragma SPARK_Mode;

   procedure Get (Result: out String;
                  Last   : out Natural;
                  Marker : String := "*")
     with
       Pre =>
         Result'Length > 0
         and Result'Last < Positive'Last;
end Hidden_Input;
