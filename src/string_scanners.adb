pragma Ada_2012;
package body String_Scanners is
   pragma SPARK_Mode;

   function Create (Input : String) return Scanner_Type
   is (Scanner_Type'(Length => Input'Length,
                     Data         => Input,
                     Cursor       => 1,
                     Saved_Cursor => 1,
                     Saved        => False));

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
      if Integer (Scanner.Cursor) > Integer (Scanner.Data'Last - Amount) then
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
      if Remaining (Scanner) < Amount then
         Scanner.Cursor := Scanner.Data'Last + 1;
      else
         Scanner.Cursor := Scanner.Cursor + Amount;
      end if;
   end Next;

   ----------
   -- Back --
   ----------

   procedure Back (Scanner : in out Scanner_Type)
   is
   begin
      if Start_Of_Input (Scanner) then
         return;
      else
         Scanner.Cursor := Scanner.Cursor - 1;
      end if;
   end Back;

   --------------------
   -- Start_Of_Input --
   --------------------

   function Start_Of_Input (Scanner : Scanner_Type) return Boolean
   is (Scanner.Cursor = Scanner.Data'First);

   -------------------
   -- Save_Position --
   -------------------

   procedure Save_Position (Scanner : in out Scanner_Type)
   is
   begin
      if Scanner.Saved then
         raise Constraint_Error;
      end if;

      Scanner.Saved_Cursor := Scanner.Cursor;
      Scanner.Saved := True;
   end Save_Position;

   ----------------------
   -- Restore_Position --
   ----------------------

   procedure Restore_Position (Scanner : in out Scanner_Type)
   is
   begin
      if not Scanner.Saved then
         raise Constraint_Error;
      end if;

      Scanner.Cursor := Scanner.Saved_Cursor;
      Scanner.Saved := False;
   end Restore_Position;

   ---------------------
   -- Forget_Position --
   ---------------------

   procedure Forget_Position (Scanner : in out Scanner_Type)
   is
   begin
      Scanner.Saved := False;
   end Forget_Position;

   --------------------
   -- Position_Saved --
   --------------------

   function Position_Saved (Scanner : Scanner_Type) return Boolean
   is (Scanner.Saved);

end String_Scanners;
