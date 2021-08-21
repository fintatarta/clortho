with Ada.Strings.Maps;

package Clortho.Password_Style.Utilities is
   pragma SPARK_Mode;

   ----------------
   -- Set_Buffer --
   ----------------
   type Set_Buffer is private;

   function Free_Space (Item : Set_Buffer) return Natural;

   procedure Append (To   : in out Set_Buffer;
                     Item : Ada.Strings.Maps.Character_Set)
     with
       Pre => Free_Space (To) > 0,
       Post => Free_Space (To) = Free_Space (To'Old) - 1;

   procedure Loop_Over (Buffer : in out Set_Buffer);

   procedure Next (Buffer : in out Set_Buffer);

   function Has_Element (Buffer : Set_Buffer) return Boolean;

   function Current_Element (Buffer : Set_Buffer) return Ada.Strings.Maps.Character_Set
     with
       Pre => Has_Element (Buffer);

   -------------
   -- Scanner --
   -------------

   type Segment_Scanner (<>) is  private;

   function Create (Input : String) return Segment_Scanner
     with
       Pre =>
         (Input'Length >= 3 and Input'Length < Integer'Last)
         and then
           (Input (Input'First) = '/'
            and Input (Input'Last) = '/'
            and Input (Input'First + 1) /= '/'
            and Input (Input'Last - 1) /= '/'),
           Post =>
             Length (Create'Result) = Input'Length - 2;

   function End_Of_Input (Item : Segment_Scanner) return Boolean;

   function Current_Segment (Item : Segment_Scanner) return String
     with
       Pre => not End_Of_Input (Item),
       Post => Current_Segment'Result'Length <= Length (Item);

   function Length (Item : Segment_Scanner) return Positive
     with Post => Length'Result >= 1 and Length'Result < Integer'Last;

   procedure Next_Segment (Item : in out Segment_Scanner)
     with Pre => not End_Of_Input (Item);

private
   subtype Set_Index is Positive;

   type Set_Array is array (Set_Index range <>) of Ada.Strings.Maps.Character_Set;

   type Set_Buffer is
      record
         Sets       : Set_Array (1 .. 256) := (others => Ada.Strings.Maps.Null_Set);
         First_Free : Set_Index := 1;
         Cursor     : Set_Index := 1;
      end record
     with
       Type_Invariant =>
         First_Free <= Sets'Last + 1
         and Cursor <= Sets'Last + 1;

   type Segment_Scanner (Len : Positive) is
      record
         Segment_First  : Positive;
         Segment_Last   : Positive;
         Data           : String (1 .. Len);
      end record
     with
       Type_Invariant =>
         Segment_Scanner.Segment_First <= Segment_Scanner.Len + 1
         and Segment_Scanner.Segment_Last > Segment_Scanner.Segment_First;
end Clortho.Password_Style.Utilities;
