pragma Ada_2012;
with Ada.Unchecked_Conversion;
with Interfaces.C.Pointers;
with GNAT.OS_Lib;

package body Clortho.IID_Drawing.Source is
   use Interfaces.C;

   ----------
   -- Fill --
   ----------

   function Random_Word return Word
   is
      subtype Byte is Interfaces.Unsigned_8;

      Byte_Per_Word : constant := Word'Size / 8;

      type Byte_Array is array (Positive range <>) of aliased Byte;

      subtype Word_Buffer is Byte_Array (1 .. Byte_Per_Word);

      function To_Word is
        new Ada.Unchecked_Conversion (Source => Word_Buffer,
                                      Target => Word);

      package Byte_Pointers is
        new Interfaces.C.Pointers (Index              => Positive,
                                   Element            => Byte,
                                   Element_Array      => Byte_Array,
                                   Default_Terminator => 0);

      type Ssize_T is range -size_t'Modulus / 2 .. size_t'Modulus / 2 - 1;

      function Get_Random (Buffer : Byte_Pointers.Pointer;
                           Length : size_t;
                           Flags  : unsigned)
                           return Ssize_T
        with Import, Convention => C,  External_Name => "getrandom";

      Err : Ssize_T;

      Buffer        : Word_Buffer;
   begin
      Err := Get_Random (Byte_Pointers.Pointer'(Buffer (Buffer'First)'Access),
                         Buffer'Length,
                         0);

      if Err < 0 then
         raise Constraint_Error with GNAT.OS_Lib.Errno_Message;
      end if;

      if Err /= Buffer'Length then
         --  This should never happen since getrandom guarantees to fill the
         --  buffer if its length is not larger than 256
         raise Program_Error;
      end if;

      return To_Word (Buffer);
   end Random_Word;
end Clortho.IID_Drawing.Source;
