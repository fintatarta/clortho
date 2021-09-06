pragma Ada_2012;
package body File_Encryption is

   ----------------------
   -- Stretch_Password --
   ----------------------

   function Stretch_Password (Password : String) return Key_Type is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Stretch_Password unimplemented");
      return
        raise Program_Error with "Unimplemented function Stretch_Password";
   end Stretch_Password;

   ------------
   -- To_Key --
   ------------

   function To_Key (Data : Key_Array) return Key_Type is
   begin
      pragma Compile_Time_Warning (Standard.True, "To_Key unimplemented");
      return raise Program_Error with "Unimplemented function To_Key";
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

end File_Encryption;
