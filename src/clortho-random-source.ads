with Interfaces;

private package Clortho.Random.Source is
   type Index is range 1 .. 256;

   subtype Buffer_Entry is Interfaces.Unsigned_32;
   type Random_Buffer is array (Index range <>) of aliased Buffer_Entry;

   procedure Fill (Data : out Random_Buffer);
end Clortho.Random.Source;
