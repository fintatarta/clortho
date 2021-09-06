pragma Ada_2022;
with Ada.Unchecked_Conversion;
with SPARKNaCl.Hashing;

package body File_Encryption is


   ----------------------
   -- Stretch_Password --
   ----------------------

   function Stretch_Password
     (Password : String; Nonce : Stream_Element_Array) return Key_Type
   is
      use SPARKNaCl;

      subtype Password_Subtype is String (Password'Range);
      subtype Password_Byte_Seq is
        SPARKNaCl.Byte_Seq (I32 (Password'First) .. I32 (Password'Last));

      function To_Byte_Seq is
        new Ada.Unchecked_Conversion (Source => Password_Subtype,
                                      Target => Password_Byte_Seq);

      subtype Nonce_Subtype is Stream_Element_Array (Nonce'Range);
      subtype Nonce_Byte_Seq is
        SPARKNaCl.Byte_Seq (I32 (Nonce'First) .. I32 (Nonce'Last));

      function To_Byte_Seq is
        new Ada.Unchecked_Conversion (Source => Nonce_Subtype,
                                      Target => Nonce_Byte_Seq);

      Stretch_Input : constant Byte_Seq :=
                        To_Byte_Seq (Password) & To_Byte_Seq (Nonce);

      Result : Hashing.Digest := Hashing.IV;
   begin
      for I in 1 .. 100_000 loop
         Hashing.Hashblocks (Result, Stretch_Input);
      end loop;

      return (K => Core.Construct (Result (1 .. 32)));
   end Stretch_Password;

   ------------
   -- To_Key --
   ------------

   function To_Key (Data : Key_Array) return Key_Type is
      function To_Bytes32 is
        new Ada.Unchecked_Conversion (Source => Key_Array,
                                      Target => SPARKNaCl.Bytes_32);
   begin
      return (K => SPARKNaCl.Core.Construct (To_Bytes32 (Data)));
   end To_Key;

   -------------------------
   -- Load_Encrypted_File --
   -------------------------

   function Load_Encrypted_File
     (Filename : String; Key : Key_Type) return Storage.Bounded.Stream_Type
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Load_Encrypted_File unimplemented");
      return
        raise Program_Error with "Unimplemented function Load_Encrypted_File";
   end Load_Encrypted_File;

   -------------------------
   -- Load_Encrypted_File --
   -------------------------

   function Load_Encrypted_File
     (Filename : String; Password : String) return Storage.Bounded.Stream_Type
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Load_Encrypted_File unimplemented");
      return
        raise Program_Error with "Unimplemented function Load_Encrypted_File";
   end Load_Encrypted_File;

   -------------------------
   -- Save_Encrypted_File --
   -------------------------

   procedure Save_Encrypted_File
     (Filename : String; Key : Key_Type;
      Data     : Storage.Storage_Stream_Type'Class)
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Save_Encrypted_File unimplemented");
      raise Program_Error with "Unimplemented procedure Save_Encrypted_File";
   end Save_Encrypted_File;

   -------------------------
   -- Save_Encrypted_File --
   -------------------------

   procedure Save_Encrypted_File
     (Filename : String; Password : String;
      Data     : Storage.Storage_Stream_Type'Class)
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Save_Encrypted_File unimplemented");
      raise Program_Error with "Unimplemented procedure Save_Encrypted_File";
   end Save_Encrypted_File;

end File_Encryption;
