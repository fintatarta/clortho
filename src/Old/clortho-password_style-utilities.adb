with Ada.Strings.Fixed;

package body Clortho.Password_Style.Utilities is
   pragma SPARK_Mode;

   function Length (Item : Segment_Scanner) return Positive
   is (Item.Len);

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

   function Create (Input : String) return Segment_Scanner
   is
      Segment_Last : Natural;
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

      declare
         Core : constant String :=  Input (Input'First + 1 .. Input'Last - 1);
      begin
         pragma Assert (Core (Core'First) /= '/');
         pragma Assert (Core (Core'Last) /= '/');

         Segment_Last := Ada.Strings.Fixed.Index (Source  => Core,
                                                  Pattern => "//",
                                                  From    => Core'First);

         pragma Assert (Segment_Last /= Core'First);

         if Segment_Last = 0 then
            Segment_Last := Core'Last;
         end if;

         return Segment_Scanner'(Len           => Core'Length,
                               Segment_First => 1,
                               Segment_Last  => Segment_Last - Core'First + 1,
                               Data          => Core);
      end;
   end Create;

   function Current_Segment (Item : Segment_Scanner) return String
   is
   begin
      return Item.Data (Item.Segment_First .. Item.Segment_Last);
   end Current_Segment;

   function End_Of_Input (Item : Segment_Scanner) return Boolean
   is (Item.Segment_First > Item.Data'Last);

   ------------------
   -- Next_Segment --
   ------------------

   procedure Next_Segment (Item : in out Segment_Scanner)
   is

   begin
      if End_Of_Input (Item) then
         raise Constraint_Error;
      end if;

      if Item.Segment_Last = Item.Data'Last then
         Item.Segment_First := Item.Data'Last + 1;
         Item.Segment_Last := Item.Segment_First + 1;
         return;
      end if;

      declare
         New_Start : constant Positive := Item.Segment_Last + 3;

         Break_At  : constant Natural :=
                       Ada.Strings.Fixed.Index (Source  => Item.Data,
                                                Pattern => "//",
                                                From    => New_Start);
      begin
         if Break_At = New_Start then
            --  This means that the original input had something like ////, that
            --  is an empty set.  This is not allowed.
            raise Parsing_Error;

         elsif Break_At = 0 then
            --  There is no // in the string. What remain is the segment
            Item.Segment_First := New_Start;
            Item.Segment_Last := Item.Data'Last;

         else
            Item.Segment_First := New_Start;
            Item.Segment_Last := Break_At - 1;

         end if;
      end;
   end Next_Segment;

end Clortho.Password_Style.Utilities;
