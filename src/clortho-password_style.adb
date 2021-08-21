pragma Ada_2012;
with Ada.Strings.Maps;

package body Clortho.Password_Style is

   subtype Set_Index is Positive;

   type Set_Array is array (Set_Index range <>) of Ada.Strings.Maps.Character_Set;

   type Set_Buffer (Length : Positive) is
      record
         Sets       : Set_Array (1 .. Length);
         First_Free : Set_Index;
         Cursor     : Set_Index;
      end record;

   procedure Append (To   : in out Set_Buffer;
                     Item : Ada.Strings.Maps.Character_Set)
     with
       Pre => To.First_Free <= To.Sets'Last,
       Post => To.First_Free = To.First_Free'Old + 1;

   procedure Loop_Over (Buffer : in out Set_Buffer);

   procedure Next (Buffer : in out Set_Buffer);

   function Has_Element (Buffer : Set_Buffer) return Boolean;

   function Current_Element (Buffer : Set_Buffer) return Ada.Strings.Maps.Character_Set
     with
       Pre => Has_Element (Buffer);

   procedure Loop_Over (Buffer : in out Set_Buffer)
   is
   begin
      Buffer.Cursor := Buffer.Sets'First;
   end Loop_Over;

   procedure Next (Buffer : in out Set_Buffer)
   is
   begin
      Buffer.Cursor := Buffer.Cursor + 1;
   end Next;

   function Has_Element (Buffer : Set_Buffer) return Boolean
   is (Buffer.Cursor < Buffer.First_Free);

   function Current_Element (Buffer : Set_Buffer) return Ada.Strings.Maps.Character_Set
   is (Buffer.Sets (Buffer.Cursor));

   procedure Append (To   : in out Set_Buffer;
                     Item : Ada.Strings.Maps.Character_Set)
   is
   begin
      To.Sets (To.First_Free) := Item;
      To.First_Free := To.First_Free + 1;
   end Append;
   -----------
   -- Parse --
   -----------

   function Parse (Input : String) return Password_Conditions.Condition_Type is
   begin
      pragma Compile_Time_Warning (Standard.True, "Parse unimplemented");
      return raise Program_Error with "Unimplemented function Parse";
   end Parse;

end Clortho.Password_Style;
