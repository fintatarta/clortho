package Clortho.Utilities with SPARK_Mode is

   function Ends_With (Item : String; Tail : String) return Boolean
     with
       Pre => Tail'Length > 0;
end Clortho.Utilities;
