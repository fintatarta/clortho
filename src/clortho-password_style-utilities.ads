with Ada.Strings.Maps;

private package Clortho.Password_Style.Utilities is

   ----------------
   -- Set_Buffer --
   ----------------
   type Set_Buffer (Length : Positive) is private;

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

   type Input_Scanner (Len : Natural) is  private;

   function Create (Input : String) return Input_Scanner
     with
       Pre =>
         Input'Length >= 3
         and then
           (Input (Input'First) = '/'
            and Input (Input'Last) = '/'
            and Input (Input'First + 1) /= '/'
            and Input (Input'Last - 1) /= '/');

   function End_Of_Input (Item : Input_Scanner) return Boolean;

   function Current_Segment (Item : Input_Scanner) return String
     with Pre => not End_Of_Input (Item);

   procedure Next_Segment (Item : in out Input_Scanner)
     with Pre => not End_Of_Input (Item);

private
   subtype Set_Index is Positive;

   type Set_Array is array (Set_Index range <>) of Ada.Strings.Maps.Character_Set;

   type Set_Buffer (Length : Positive) is
      record
         Sets       : Set_Array (1 .. Length) := (others => Ada.Strings.Maps.Null_Set);
         First_Free : Set_Index := 1;
         Cursor     : Set_Index := 1;
      end record;

   type Input_Scanner (Len : Natural) is
      record
         Segment_First  : Positive;
         Segment_Last   : Positive;
         Data           : String (1 .. Len);
      end record
     with
       Type_Invariant =>
         Input_Scanner.Segment_First <= Input_Scanner.Len + 1
         and Input_Scanner.Segment_Last > Input_Scanner.Segment_First;
end Clortho.Password_Style.Utilities;
