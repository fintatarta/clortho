pragma Ada_2012;
with Ada.Strings.Fixed;

--  with Ada.Text_IO; use Ada.Text_IO;

package body Text_Scanners is
   pragma SPARK_Mode;

   function New_Scanner (Max_Length : Positive) return Input_Reader
   is (Input_Reader'(Max_Length => Max_Length,
                     Input      => (others => ASCII.NUL),
                     Status     => Reader_Status'(Input_Length => 0,
                                                  Cursor       => 0,
                                                  Unused       => False)));
   -----------
   -- Reset --
   -----------

   procedure Reset (Reader : out Input_Reader; Input : String) is
   begin
      if Input'Length > Reader.Max_Length then
         raise Constraint_Error;
      end if;

      Reader.Status := Reader_Status'(Input_Length => Input'Length,
                                      Cursor       => 0,
                                      Unused       => False);

      Ada.Strings.Fixed.Move (Source  => Input,
                              Target  => Reader.Input,
                              Justify => Ada.Strings.Left,
                              Pad     => ASCII.NUL);
   end Reset;

   ----------------------------
   -- Is_Current_Char_Unused --
   ----------------------------

   function Is_Current_Char_Unused (Reader : Input_Reader) return Boolean
   is (Reader.Status.Unused);

   ------------------
   -- End_Of_Input --
   ------------------

   function End_Of_Input (Reader : Input_Reader) return Boolean
   is (Reader.Status.Cursor > Reader.Status.Input_Length);

   --------------------
   -- Begin_Of_Input --
   --------------------

   function Begin_Of_Input (Reader : Input_Reader) return Boolean
   is (Reader.Status.Cursor = Reader.Input'First);

   ------------------
   -- Current_Char --
   ------------------

   procedure Current_Char (Reader : in out Input_Reader;
                           Char   : out Character)
   is
   begin
      if End_Of_Input (Reader) then
         raise Constraint_Error;
      end if;

      --  Put_Line ("Current char " & Reader.Status.Cursor'Image);
      Char := Reader.Input (Reader.Status.Cursor);
      Reader.Status.Unused := False;
   end Current_Char;

   ------------
   -- Ungetc --
   ------------

   procedure Ungetc (Reader : in out Input_Reader) is
   begin
      if Begin_Of_Input (Reader) then
         raise Constraint_Error;
      end if;

      Reader.Status.Cursor := Reader.Status.Cursor - 1;
      Reader.Status.Unused := False;
   end Ungetc;

   --------------------
   -- Next_Character --
   --------------------

   procedure Next_Character (Reader : in out Input_Reader)
   is
   begin
      if End_Of_Input (Reader) then
         raise Constraint_Error;
      end if;
      --  Put_Line ("Next " & Reader.Status.Cursor'Image);
      Reader.Status.Cursor := Reader.Status.Cursor + 1;
      Reader.Status.Unused := True;

      --  Put_Line (Reader.Status.Cursor'Image);
   end Next_Character;

   ---------------
   -- Remaining --
   ---------------

   function Remaining (Reader : Input_Reader) return Natural
   is (Reader.Status.Input_Length - (Reader.Status.Cursor - 1));

end Text_Scanners;
