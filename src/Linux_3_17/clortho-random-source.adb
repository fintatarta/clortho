pragma Ada_2012;
with Ada.Unchecked_Conversion;
with Interfaces.C.Pointers;
with GNAT.OS_Lib;

package body Clortho.Random.Source is
   use Interfaces.C;

   ----------
   -- Fill --
   ----------

   function Random_Word return Word
   is
      subtype Byte is Interfaces.Unsigned_8;

      type Byte_Array is array (Positive range <>) of aliased Byte;

      package Byte_Pointers is
        new Interfaces.C.Pointers (Index              => Positive,
                                   Element            => Interfaces.Unsigned_8,
                                   Element_Array      => Byte_Array,
                                   Default_Terminator => 0);

      Byte_Per_Word : constant := Word'Size / 8;

      type Ssize_T is range -size_t'Modulus / 2 .. size_t'Modulus / 2 - 1;

      function Get_Random (Buffer : Byte_Pointers.Pointer;
                           Length : size_t)
                           return Ssize_T
        with Import, Convention => C,  External_Name => "getrandom";

      Err : Ssize_T;

      subtype Buffer_Type is Byte_Array (1 .. Byte_Per_Word);

      function to_word is
        new Ada.Unchecked_Conversion (Source => Buffer_Type,
                                      Target => Word);

      Buffer        : Buffer_Type;
   begin
      Err := Get_Random (Byte_Pointers.Pointer'(Buffer (Buffer'First)'Access),
                         Buffer'Length);

      if Err < 0 then
         raise Constraint_Error with GNAT.OS_Lib.Errno_Message;
      end if;

      if Err /= Buffer'Length then
         --  This should never happen since getrandom guarantees to fill the
         --  buffer if its length is not larger than 256
         raise Program_Error;
      end if;

      return to_word (Buffer);
   end Random_Word;
end Clortho.Random.Source;
