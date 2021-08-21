with Ada.Strings.Fixed;

package body Clortho.Password_Style.Utilities is

   function Free_Space (Item : Set_Buffer) return Natural
   is (Item.Sets'Last - Item.First_Free + 1);

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

   function Create (Input : String) return Input_Scanner
   is
   begin
      if Input'Length < 3
        or else
          (Input (Input'First) /= '/'
           or Input (Input'Last) /= '/'
           or Input (Input'First + 1) = '/'
           or Input (Input'Last - 1) = '/')
      then
         raise Parsing_Error;
      end if;

      return Input_Scanner'(Len    => Input'Length - 2,
                            Cursor => 1,
                            Data   => Input (Input'First + 1 .. Input'Last - 1));
   end Create;

   function End_Of_Input (Item : Input_Scanner) return Boolean
   is (Item.Cursor > Item.Data'Last);

   function Next_Segment (Item : in out Input_Scanner) return String
   is
      Break_At : constant Natural :=
                   Ada.Strings.Fixed.Index (Source  => Item.Data,
                                            Pattern => "//",
                                            From    => Item.Cursor);

      Start    : constant Positive := Item.Cursor;
   begin
      if Break_At = Item.Cursor then
         --  This means that the original input had something like ////, that
         --  is an empty set.  This is not allowed.
         raise Parsing_Error;

      elsif Break_At = 0 then
         --  There is no // in the string. What remain is the segment
         Item.Cursor := Item.Data'Last + 1;
         return Item.Data (Start .. Item.Data'Last);

      else
         Item.Cursor := Break_At + 2;
         return Item.Data (Start .. Break_At - 1);
      end if;
   end Next_Segment;

end Clortho.Password_Style.Utilities;
