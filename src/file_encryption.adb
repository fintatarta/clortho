pragma Ada_2022;
with Ada.Unchecked_Conversion;
with SPARKNaCl.Hashing;
with SPARKNaCl.Stream;

package body File_Encryption is
   use SPARKNaCl;

   function To_Byte_Seq (X : String) return Byte_Seq;

   function To_Byte_Seq (X : Stream_Element_Array) return Byte_Seq;

   function To_Stream_Array (X : Byte_Seq) return Stream_Element_Array;

   -----------------
   -- To_Byte_Seq --
   -----------------

   function To_Byte_Seq (X : String) return Byte_Seq
   is
      subtype Source is String (X'Range);
      subtype Target is Byte_Seq (1 .. Source'Length);

      function Convert is
        new Ada.Unchecked_Conversion (Source => Source,
                                      Target => Target);
   begin
      return Convert (X);
   end To_Byte_Seq;

   -----------------
   -- To_Byte_Seq --
   -----------------

   function To_Byte_Seq (X : Stream_Element_Array) return Byte_Seq
   is
      subtype Source is Stream_Element_Array (X'Range);
      subtype Target is Byte_Seq (1 .. Source'Length);

      function Convert is
        new Ada.Unchecked_Conversion (Source => Source,
                                      Target => Target);
   begin
      return Convert (X);
   end To_Byte_Seq;

   ---------------------
   -- To_Stream_Array --
   ---------------------

   function To_Stream_Array (X : Byte_Seq) return Stream_Element_Array
   is
      subtype Target is Stream_Element_Array (1 .. X'Length);
      subtype Source is Byte_Seq (1 .. X'Length);

      function Convert is
        new Ada.Unchecked_Conversion (Source => Source,
                                      Target => Target);
   begin
      return Convert (X);
   end To_Stream_Array;

   ----------------------
   -- Stretch_Password --
   ----------------------

   function Stretch_Password
     (Password : String; Nonce : Stream_Element_Array) return Key_Type
   is
      --  subtype Password_Subtype is String (Password'Range);
      --  subtype Password_Byte_Seq is
      --    SPARKNaCl.Byte_Seq (I32 (Password'First) .. I32 (Password'Last));
      --
      --  function To_Byte_Seq is
      --    new Ada.Unchecked_Conversion (Source => Password_Subtype,
      --                                  Target => Password_Byte_Seq);
      --
      --  subtype Nonce_Subtype is Stream_Element_Array (Nonce'Range);
      --  subtype Nonce_Byte_Seq is
      --    SPARKNaCl.Byte_Seq (I32 (Nonce'First) .. I32 (Nonce'Last));
      --
      --  function To_Byte_Seq is
      --    new Ada.Unchecked_Conversion (Source => Nonce_Subtype,
      --                                  Target => Nonce_Byte_Seq);

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
     (Stream   : Stream_IO.Stream_Access;
      Size     : Stream_IO.Count;
      Key      : Key_Type)
      return Storage.Bounded.Stream_Type
   is
      use SPARKNaCl.Stream;

      Encrypted : Stream_Element_Array (1 .. Stream_Element_Count (Size));
      Last       : Stream_Element_Offset;
      Nonce      : Salsa20_Nonce;
   begin
      Salsa20_Nonce'Read (Stream, Nonce);

      Read (Stream => Stream.all,
            Item   => Encrypted,
            Last   => Last);

      declare
         Src        : constant Byte_Seq := To_Byte_Seq (Encrypted);
         Dst        : Byte_Seq (Src'Range);
         Clear_Text : Stream_Element_Array (Encrypted'Range);
      begin
         SPARKNaCl.Stream.Salsa20_Xor (C => Dst,
                                       M => Src,
                                       N => Nonce,
                                       K => Key.K);

         Clear_Text := To_Stream_Array (Dst);

         return Result : Storage.Bounded.Stream_Type (Clear_Text'Length) do
            Storage.Bounded.Write (Stream => Result,
                                   Item   => Clear_Text);
         end return;
      end;
   end Load_Encrypted_File;

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
