with Ada.Sequential_IO;
with Ada.Text_IO; use Ada.Text_IO;
with Clortho.IID_Drawing.Source.Random_Device;

package body Clortho.IID_Drawing.Source is
   package Word_Io is
     new Ada.Sequential_IO (Word);

   Random_Source : Word_Io.File_Type;

   function Random_Word return Word
   is
      Result : Word;
   begin
      pragma Assert (Word_Io.Is_Open (Random_Source));

      Word_Io.Read (Random_Source, Result);

      return Result;
   end Random_Word;

begin
   Word_Io.Open (File => Random_Source,
                 Mode => Word_Io.In_File,
                 Name => Random_Device.Name);
end Clortho.IID_Drawing.Source;
