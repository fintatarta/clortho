pragma Ada_2022;
with Ada.Unchecked_Conversion;
with SPARKNaCl.Hashing;
with SPARKNaCl.Stream;

package body File_Encryption is
   pragma SPARK_Mode;
   use SPARKNaCl;

   function To_Byte_Seq (X : String) return Byte_Seq;

   function To_Byte_Seq (X     : Stream_Element_Array;
                         First : I32 := 0) return Byte_Seq;

   function To_Stream_Array (X : Byte_Seq) return Stream_Element_Array;

   Password_Nonce_Size : constant Stream_Element_Count := 512 / 8;

   subtype Password_Nonce_Type is Stream_Element_Array (1 .. Password_Nonce_Size);

   function Get_Salsa20_Nonce return SPARKNaCl.Stream.Salsa20_Nonce;

   function Get_Password_Nonce  return Password_Nonce_Type;

   function Get_Password_Nonce  return Password_Nonce_Type
   is
      use type I32;

      Tmp : Byte_Seq (0 .. I32 (Password_Nonce_Size) - 1);
   begin
      Random_Bytes (Tmp);

      return To_Stream_Array (Tmp);
   end Get_Password_Nonce;

   function Get_Salsa20_Nonce return SPARKNaCl.Stream.Salsa20_Nonce
   is
      Result : SPARKNaCl.Stream.Salsa20_Nonce;
   begin
      Random_Bytes (Byte_Seq (Result));
      return Result;
   end Get_Salsa20_Nonce;

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

   function To_Byte_Seq (X     : Stream_Element_Array;
                         First : I32 := 0) return Byte_Seq
   is
      use type I32;

      subtype Source is Stream_Element_Array (X'Range);
      subtype Target is Byte_Seq (First .. First + Source'Length - 1);

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
      pragma SPARK_Mode (Off);
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

   function Load_Encrypted_File (Filename : String;
                                 Key      : Key_Type)
                                 return Storage.Bounded.Stream_Type
   is
      pragma SPARK_Mode (Off);

      Input : Stream_IO.File_Type;
   begin
      Stream_IO.Open (File => Input,
                      Mode => Stream_IO.In_File,
                      Name => Filename);

      return Result : constant Storage.Bounded.Stream_Type :=
        Load_Encrypted_File (Stream => Stream_IO.Stream (Input),
                             Size   => Stream_IO.Size (Input),
                             Key    => Key)
      do
         Stream_IO.Close (Input);
      end return;
   end Load_Encrypted_File;

   -------------------------
   -- Load_Encrypted_File --
   -------------------------

   function Load_Encrypted_File (Filename : String;
                                 Password : String)
                                 return Storage.Bounded.Stream_Type
   is
      pragma SPARK_Mode (Off);

      Input  : Stream_IO.File_Type;
      Stream : Stream_IO.Stream_Access;
      Nonce  : Password_Nonce_Type;
   begin
      Stream_IO.Open (File => Input,
                      Mode => Stream_IO.In_File,
                      Name => Filename);

      Stream := Stream_IO.Stream (Input);

      Password_Nonce_Type'Read (Stream, Nonce);

      declare
         Key : constant Key_Type := Stretch_Password (Password, Nonce);
      begin
         return Result : constant Storage.Bounded.Stream_Type :=
           Load_Encrypted_File (Stream => Stream,
                                Size   => Stream_IO.Size (Input),
                                Key    => Key)
         do
            Stream_IO.Close (Input);
         end return;
      end;
   end Load_Encrypted_File;

   procedure Save_Encrypted
     (Stream : Stream_IO.Stream_Access;
      Key    : Key_Type;
      Data   : in out Storage.Storage_Stream_Type'Class)
   is
      pragma SPARK_Mode (Off);

      use SPARKNaCl.Stream;

      Nonce : constant Salsa20_Nonce := Get_Salsa20_Nonce;
      Buffer : Stream_Element_Array (1 .. Storage.Element_Count (Data));
      Last : Stream_Element_Offset;
   begin
      Data.Read (Item => Buffer,
                 Last => Last);

      pragma Assert (Buffer'Last = Last);

      declare
         Cleartext : constant Byte_Seq := To_Byte_Seq (Buffer);
         Encrypted : Byte_Seq (Cleartext'Range);
      begin
         SPARKNaCl.Stream.Salsa20_Xor (C => Encrypted,
                                       M => Cleartext,
                                       N => Nonce,
                                       K => Key.K);

         Salsa20_Nonce'Write (Stream, Nonce);

         Byte_Seq'Write (Stream, Encrypted);
      end;
   end Save_Encrypted;

   -------------------------
   -- Save_Encrypted_File --
   -------------------------

   procedure Save_Encrypted_File
     (Filename : String; Key : Key_Type;
      Data     : in out Storage.Storage_Stream_Type'Class)
   is
      pragma SPARK_Mode (Off);

      Target : Stream_IO.File_Type;
   begin
      Stream_IO.Open (File => Target,
                      Mode => Stream_IO.Out_File,
                      Name => Filename);

      Save_Encrypted (Stream => Stream_IO.Stream (Target),
                      Key    => Key,
                      Data   => Data);

      Stream_IO.Close (Target);
   end Save_Encrypted_File;

   -------------------------
   -- Save_Encrypted_File --
   -------------------------

   procedure Save_Encrypted_File
     (Filename : String;
      Password : String;
      Data     : in out Storage.Storage_Stream_Type'Class)
   is
      pragma SPARK_Mode (Off);

      Nonce : constant Password_Nonce_Type := Get_Password_Nonce;
      Key   : constant Key_Type := Stretch_Password (Password, Nonce);
      Output : Stream_IO.File_Type;
      Stream : Stream_IO.Stream_Access;
   begin
      Stream_IO.Open (File => Output,
                      Mode => Stream_IO.Out_File,
                      Name => Filename);

      Stream := Stream_IO.Stream (Output);

      Password_Nonce_Type'Write (Stream, Nonce);
      Save_Encrypted (Stream, Key, Data);

      Stream_IO.Close (Output);
   end Save_Encrypted_File;

end File_Encryption;
