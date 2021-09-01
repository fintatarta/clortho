private package Clortho.Command_Line.Option_Scanning is
   pragma SPARK_Mode;

   type Option_Descriptor is private;

   procedure Scan_Options (Cursor : out Positive;
                           Result : out Option_Descriptor;
                           Err    : out Error_Status);

private
   type Option_Descriptor is
      record
         To_Do                  : Command_Type;
         Back_Step              : Natural;
         Password_Len           : Natural;
         Password_N_Bits        : Natural;
         User_Provided_Password : Unbounded_String;
         Output_Target          : Target_Name;
         Specs                  : Unbounded_String;
         Use_Standard_Input     : Boolean;
      end record;
end Clortho.Command_Line.Option_Scanning;
