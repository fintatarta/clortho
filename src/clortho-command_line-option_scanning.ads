with Clortho.Command_Line.Option_Sets;

private package Clortho.Command_Line.Option_Scanning is
   pragma SPARK_Mode;

   procedure Scan_Options (Cursor : out Positive;
                           Result : in out Option_Sets.Option_Set;
                           Err    : out Option_Processing_Error);
end Clortho.Command_Line.Option_Scanning;
