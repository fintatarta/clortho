pragma Ada_2012;
with Clortho.Random.Source;
with Ada.Numerics.Elementary_Functions;

package body Clortho.Random is

   ----------
   -- Fill --
   ----------

   procedure Fill (Data : out Random_Data; Max : Positive) is
      use Source;

      use Ada.Numerics.Elementary_Functions;

      Buffer_Length : constant Positive := Positive
        (Float'Floor (Float (Buffer_Entry'Size) / Log (Float (Max), 2.0)));

      Buffer : Random_Buffer (1 .. Buffer_Length);
      Dst : Index := Data'First;
   begin
      while Dst < Data'Last loop
         Source.Fill (Buffer);
      end loop;
   end Fill;

end Clortho.Random;
