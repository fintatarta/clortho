pragma Ada_2022;
with Ada.Streams.Storage.Bounded;   use Ada.Streams;

private with SPARKNaCl.Core;

package File_Encryption is
   type Key_Type is limited private;

   Key_Nbits : constant Positive := 256;

   Key_Stream_Size : constant Stream_Element_Offset :=
                       Stream_Element_Offset (Key_Nbits / Stream_Element'Size);

   subtype Key_Array is Stream_Element_Array (1 .. Key_Stream_Size);

   function Stretch_Password
     (Password : String;
      Nonce    : Stream_Element_Array)
      return Key_Type;

   function To_Key (Data : Key_Array) return Key_Type;

   function Load_Encrypted_File
     (Filename : String;
      Key      : Key_Type)
      return Storage.Bounded.Stream_Type;

   function Load_Encrypted_File
     (Filename : String;
      Password : String)
      return Storage.Bounded.Stream_Type;

   procedure Save_Encrypted_File
     (Filename : String;
      Key      : Key_Type;
      Data     : Storage.Storage_Stream_Type'Class);

   procedure Save_Encrypted_File
     (Filename : String;
      Password : String;
      Data     : Storage.Storage_Stream_Type'Class);
private
   type Key_Type is
      record
         K : SPARKNaCl.Core.Salsa20_Key;
      end record;

   Key_Bit_Size : constant Positive := 256;
end File_Encryption;
