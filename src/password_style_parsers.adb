pragma Ada_2012;
with Ada.Text_IO; use Ada.Text_IO;
with Password_Style_Scanner;
with Ada.Strings.Unbounded;
with Ada.Strings.Maps.Constants;

package body Password_Style_Parsers is

   -----------
   -- Parse --
   -----------

   function Parse (Input : String) return Parsing_Result is
      use Password_Style_Scanner;
      use type Ada.Strings.Maps.Character_Set;

      package Saving_Buffer is
         Buffer : Character;
         Full   : Boolean := False;

         procedure Save (C : Character)
           with
             Pre => not Full, Post => Full;

         pragma Warnings (Off, "postcondition does not mention function result");
         function Get return Character
           with
             Pre => Full, Post => not Full;
      end Saving_Buffer;

      package body Saving_Buffer is
         procedure Save (C : Character) is
         begin
            Buffer := C;
            Full := True;
         end Save;

         function Get return Character
         is
         begin
            Full := False;
            return Buffer;
         end Get;
      end Saving_Buffer;

      function Count_Slashes (Input : String) return Positive;
      procedure Add_To_Mandatory (Set : Ada.Strings.Maps.Character_Set);
      function Mandatory_Sets return Set_Array;
      procedure Add_To_Prohibited (X : Ada.Strings.Maps.Character_Set);

      function Count_Slashes (Input : String) return Positive
      is
         Result : Natural := 0;
      begin
         for C of Input loop
            if C = '/' then
               Result := Result + 1;
            end if;
         end loop;

         return Result;
      end Count_Slashes;

      Scanner : Scanning.Automata_Type := New_Password_Style_Scanner;

      Prohibited : Ada.Strings.Maps.Character_Set := Ada.Strings.Maps.Null_Set;
      Mandatory  : Set_Array (1 .. Count_Slashes (Input));
      First_Free : Positive := Mandatory'First;

      procedure Add_To_Mandatory (Set : Ada.Strings.Maps.Character_Set) is
      begin
         if First_Free > Mandatory'Last then
            raise Constraint_Error;
         end if;

         Mandatory (First_Free) := Set;
         First_Free := First_Free + 1;
      end Add_To_Mandatory;

      function Mandatory_Sets return Set_Array
      is (Mandatory (Mandatory'First .. First_Free - 1));

      procedure Add_To_Prohibited (X : Ada.Strings.Maps.Character_Set) is
      begin
         Prohibited := Prohibited or X;
      end Add_To_Prohibited;
   begin
      if Count_Slashes (Input) < 2 then
         return Parsing_Result'(Success => Parsing_Error, N_Sets => 0);
      end if;

      Scanning.Reset (Scanner, Input);

      declare
         use Ada.Strings.Maps;

         Char         : Character;
         Action       : Action_Type;
         Buffer       : Character_Set := Ada.Strings.Maps.Null_Set;
         Prohibited   : Boolean := False;
         Complemented : Boolean := False;

         procedure Add_Single_Char (C : Character);
         procedure Add_Range (From, To : Character);
         procedure Reset_Builder;
         procedure Mark_As_Prohibited;
         procedure Mark_As_Complemented;
         procedure Close_Current_Set;

         procedure Add_Single_Char (C : Character)
         is
         begin
            Buffer := Buffer or To_Set (C);
         end Add_Single_Char;

         procedure Add_Range (From, To : Character)
         is
         begin
            Buffer := Buffer or To_Set (Character_Range'(From, To));
         end Add_Range;

         procedure Reset_Builder is
         begin
            Buffer := Null_Set;
            Prohibited := False;
            Complemented := False;
         end Reset_Builder;

         procedure Close_Current_Set is
         begin
            if Complemented then
               Buffer := not Buffer;
            end if;

            if Prohibited then
               Add_To_Prohibited (Buffer);
            else
               Add_To_Mandatory (Buffer);
            end if;

            Reset_Builder;
         end Close_Current_Set;

         procedure Mark_As_Prohibited is
         begin
            Prohibited := True;
         end Mark_As_Prohibited;

         procedure Mark_As_Complemented is
         begin
            Complemented := True;
         end Mark_As_Complemented;
      begin
         loop
            Scanning.Next (Automata => Scanner,
                           Action   => Action,
                           Char     => Char);

            case Action is
            when Nothing =>
               null;

            when Error =>
               raise Constraint_Error;

            when Initialize =>
               Reset_Builder;

            when Prohibited_Set =>
               Mark_As_Prohibited;

            when Complement_Set =>
               Mark_As_Complemented;

            when Save_Char =>
               Saving_Buffer.Save (Char);

            when Add_Saved =>
               Add_Single_Char (Saving_Buffer.Get);

            when Add_Current_Char =>
               Add_Single_Char (Char);

            when Add_Saved_And_Save =>
               Add_Single_Char (Saving_Buffer.Get);
               Saving_Buffer.Save (Char);

            when Add_Range =>
               Add_Range (Saving_Buffer.Get, Char);

            when Close_Set =>
               Close_Current_Set;

            when End_Of_Parsing =>
               Close_Current_Set;
               exit;
            end case;
         end loop;
      end;

      return Parsing_Result'(Success => Ok,
                             N_Sets  => First_Free - 1,
                             Style   => (N_Sets     => First_Free  - 1,
                                         Prohibited => Prohibited,
                                         Mandatory  => Mandatory_Sets));
   end Parse;

   -----------
   -- Image --
   -----------

   function Image (Descr : Password_Style_Descriptor) return String
   is
      use Ada.Strings.Maps;
      use Ada.Strings.Unbounded;

      Buffer : Unbounded_String := Null_Unbounded_String;
   begin
      if Descr.Prohibited /= Null_Set then
         Buffer := Buffer & "[" & To_Sequence (Descr.Prohibited and Constants.Graphic_Set) & "] ";
      end if;

      for Set of Descr.Mandatory loop
         Buffer := Buffer & "<" & To_Sequence (Set and Constants.Graphic_Set) & ">";
      end loop;

      return To_String (Buffer);
   end Image;

   -------------
   -- Missing --
   -------------

   function Missing (Descr : Password_Style_Descriptor) return Ada.Strings.Maps.Character_Set
   is
      use type Ada.Strings.Maps.Character_Set;

      Union : Ada.Strings.Maps.Character_Set := Descr.Prohibited;
   begin
      for Set of Descr.Mandatory loop
         Union := Union or Set;
      end loop;

      return not Union;
   end Missing;

   --------------
   -- Is_Valid --
   --------------

   function Is_Valid (Descr : Password_Style_Descriptor) return Boolean
   is
      use Ada.Strings.Maps;
   begin
      for I in Descr.Mandatory'Range loop
         Put_Line (I'Image & ": " & To_Sequence (Descr.Mandatory (I)));
         if (Descr.Mandatory (I) and Descr.Prohibited) /= Null_Set then
            Put_Line ("!!!" & To_Sequence (Descr.Prohibited));

            return False;
         end if;

         for J in I + 1 .. Descr.Mandatory'Last loop
            if (Descr.Mandatory (I) and Descr.Prohibited) /= Null_Set then
               Put_Line ("!!!" & J'Image & ": " & To_Sequence (Descr.Mandatory (J)));
               return False;
            end if;
         end loop;
      end loop;

      return True;
   end Is_Valid;

end Password_Style_Parsers;
