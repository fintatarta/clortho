package Clortho.Random is
   type Random_Data is array (Positive range <>) of Positive;

   procedure Fill (Data : out Random_Data;
                   Max  : Positive)
     with
       Post => (for all N of Data => (N >= 1 and N <= Max));
end Clortho.Random;
