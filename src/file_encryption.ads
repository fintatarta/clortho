pragma Ada_2022;
with Ada.Streams.Storage.Bounded;   use Ada.Streams;
with Ada.Streams.Stream_IO;

private with SPARKNaCl.Core;

package File_Encryption is
   pragma SPARK_Mode;
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
      return Storage.Bounded.Stream_Type
     with SPARK_Mode => Off;

   function Load_Encrypted_File
     (Stream   : Stream_IO.Stream_Access;
      Size     : Stream_IO.Count;
      Key      : Key_Type)
      return Storage.Bounded.Stream_Type;

   function Load_Encrypted_File
     (Filename : String;
      Password : String)
      return Storage.Bounded.Stream_Type;

   procedure Save_Encrypted_File
     (Filename : String;
      Key      : Key_Type;
      Data     : in out Storage.Storage_Stream_Type'Class);

   procedure Save_Encrypted_File
     (Filename : String;
      Password : String;
      Data     : in out Storage.Storage_Stream_Type'Class);

   procedure Save_Encrypted
     (Stream : Stream_IO.Stream_Access;
      Key    : Key_Type;
      Data   : in out Storage.Storage_Stream_Type'Class);

private
   type Key_Type  is
      record
         K : SPARKNaCl.Core.Salsa20_Key;
      end record;

   Key_Bit_Size : constant Positive := 256;
end File_Encryption;
