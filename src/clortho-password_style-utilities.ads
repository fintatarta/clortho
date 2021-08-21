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

   function Next_Segment (Item : in out Input_Scanner) return String
     with Pre => not End_Of_Input (Item);

private
   subtype Set_Index is Positive;

   type Set_Array is array (Set_Index range <>) of Ada.Strings.Maps.Character_Set;

   type Set_Buffer (Length : Positive) is
      record
         Sets       : Set_Array (1 .. Length);
         First_Free : Set_Index;
         Cursor     : Set_Index;
      end record;

   type Input_Scanner (Len : Natural) is
      record
         Cursor : Positive;
         Data   : String (1 .. Len);
      end record;

end Clortho.Password_Style.Utilities;
