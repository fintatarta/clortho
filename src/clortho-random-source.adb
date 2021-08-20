pragma Ada_2012;
with Ada.Unchecked_Conversion;
with Interfaces.C.Pointers;

package body Clortho.Random.Source is
   use Interfaces.C;

   ----------
   -- Fill --
   ----------

   procedure Fill (Data : out Random_Buffer) is

      type Byte_Array is array (Positive range <>) of
        aliased Interfaces.Unsigned_8;

      Byte_Per_Buffer_Entry : constant := Buffer_Entry'Size / 8;

      package Byte_Pointers is
        new Interfaces.C.Pointers (Index              => Positive,
                                   Element            => Interfaces.Unsigned_8,
                                   Element_Array      => Byte_Array,
                                   Default_Terminator => 0);

      type Ssize_T is range -size_t'Modulus / 2 .. size_t'Modulus / 2 - 1;

      function Get_Random (Buffer : Byte_Pointers.Pointer;
                           Length : size_t)
                        return Ssize_T
        with Import, Convention => C,  External_Name => "getrandom";

      Err : Ssize_T;

      Buffer : Byte_Array (1 .. Data'Length * Byte_Per_Buffer_Entry);
   begin
      Err := Get_Random (Byte_Pointers.Pointer'(Buffer (Buffer'First)'Access),
                         Buffer'Length);

      if Err < 0 then
         raise Constraint_Error;
      end if;

      if Err /= Buffer'Length then
         --  This should never happen since getrandom guarantees to fill the
         --  buffer if its length is not larger than 256
         raise Program_Error;
      end if;

      declare
         subtype Mini_Buffer is Byte_Array (1 .. Byte_Per_Buffer_Entry);

         function Convert is
           new Ada.Unchecked_Conversion (Source => Mini_Buffer,
                                         Target => Buffer_Entry);

         Src : Positive := Buffer'First;
      begin
         for I in Data'Range loop
            pragma Loop_Invariant
              (Src =  Integer (I - Data'First) * Byte_Per_Buffer_Entry + Buffer'First);

            Data (I) := Convert (Buffer (Src .. Src + Byte_Per_Buffer_Entry - 1));
            Src := Src + Byte_Per_Buffer_Entry;
         end loop;
      end;
   end Fill;

end Clortho.Random.Source;
