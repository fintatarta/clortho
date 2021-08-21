pragma Ada_2012;
package body String_Scanners is

   function Create (Input : String) return Scanner_Type
   is (Scanner_Type'(Length => Input'Length,
                     Data   => Input,
                     Cursor => 1));

   function Remaining (Scanner : Scanner_Type) return Natural
   is ((Scanner.Data'Last + 1) - Scanner.Cursor);

   function Peek_Ahead (Scanner : Scanner_Type;
                        Amount  : Positive := 1)
                        return Character
   is
   begin
      if Scanner.Cursor + Amount > Scanner.Data'Last then
         raise Constraint_Error;
      else
         return Scanner.Data (Scanner.Cursor + Amount);
      end if;
   end Peek_Ahead;

   ----------------
   -- Peek_Ahead --
   ----------------

   function Peek_Ahead (Scanner : Scanner_Type;
                        EOF     : Character;
                        Amount  : Positive := 1)
                        return Character
   is
   begin
      if Scanner.Cursor + Amount > Scanner.Data'Last then
         return EOF;
      else
         return Scanner.Data (Scanner.Cursor + Amount);
      end if;
   end Peek_Ahead;

   ------------------
   -- End_Of_Input --
   ------------------

   function End_Of_Input (Scanner : Scanner_Type) return Boolean
   is (Scanner.Cursor > Scanner.Data'Last);

   ------------------
   -- Current_Char --
   ------------------

   function Current_Char (Scanner : Scanner_Type) return Character is
   begin
      if End_Of_Input (Scanner) then
         raise Constraint_Error;
      else
         return Scanner.Data (Scanner.Cursor);
      end if;
   end Current_Char;

   ----------
   -- Next --
   ----------

   procedure Next (Scanner : in out Scanner_Type;
                   Amount  : Positive := 1) is
   begin
      if Scanner.Remaining < Amount then
         Scanner.Cursor := Scanner.Data'Last + 1;
      else
         Scanner.Cursor := Scanner.Cursor + Amount;
      end if;
   end Next;

   ----------------------
   -- Current_Position --
   ----------------------

   function Current_Position (Scanner : Scanner_Type) return Cursor_Type
   is (Cursor_Type (Scanner.Cursor));

   ----------
   -- Seek --
   ----------

   procedure Seek (Scanner : in out Scanner_Type; Cursor : Cursor_Type) is
   begin
      Scanner.Cursor := Positive (Cursor);
   end Seek;

end String_Scanners;
