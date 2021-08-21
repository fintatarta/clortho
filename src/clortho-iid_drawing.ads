
package Clortho.IID_Drawing is
   type Random_Data is array (Positive range <>) of Positive;

   --
   --  Fill Data with integers between Min and Max randomly generated
   --  using (if possible) a cryptographically strong source
   --
   procedure Fill (Data : out Random_Data;
                   Max  : Positive;
                   Min  : Positive := 1)
     with
       Pre => Max >= Min,
       Post => (for all N of Data => (N >= Min and N <= Max));
end Clortho.IID_Drawing;
