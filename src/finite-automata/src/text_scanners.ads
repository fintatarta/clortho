package Text_Scanners is
   pragma SPARK_Mode;

   type Input_Reader (Max_Length : Positive) is private;

   function New_Scanner (Max_Length : Positive) return Input_Reader
     with
       Post => New_Scanner'Result.Max_Length = Max_Length;

   procedure Reset (Reader : out Input_Reader;
                    Input  : String)
     with
       Pre => Input'Length <= Reader.Max_Length,
       Post => not Is_Current_Char_Unused (Reader);

   function Is_Current_Char_Unused (Reader : Input_Reader) return Boolean;

   function End_Of_Input (Reader : Input_Reader) return Boolean
     with
       Post => End_Of_Input'Result = (Remaining (Reader) = 0);

   function Begin_Of_Input (Reader : Input_Reader) return Boolean;

   function Remaining (Reader : Input_Reader) return Natural;

   procedure Current_Char (Reader : in out Input_Reader;
                           Char   : out Character)
     with
       Pre =>
         not End_Of_Input (Reader),
         Post =>
           not Is_Current_Char_Unused (Reader)
           and Remaining (Reader) = Remaining (Reader'Old);

   procedure Ungetc (Reader : in out Input_Reader)
     with
       Pre =>
         not Begin_Of_Input (Reader),
         Post =>
           not Is_Current_Char_Unused (Reader)
           and Remaining (Reader) - 1 = Remaining (Reader'Old);

   procedure Next_Character (Reader : in out Input_Reader)
     with
       Pre => not End_Of_Input (Reader) and not Is_Current_Char_Unused (Reader),
     Post =>
       Is_Current_Char_Unused (Reader) and
       Remaining (Reader) = Remaining (Reader'Old) - 1;

private
   type Reader_Status is
      record
         Input_Length  : Natural := 0;
         Cursor        : Natural := 0;
         Unused        : Boolean := False;
      end record;

   type Input_Reader (Max_Length : Positive) is
      record
         Input  : String (1 .. Max_Length) := (others => ASCII.NUL);
         Status : Reader_Status;
      end record;

end Text_Scanners;
