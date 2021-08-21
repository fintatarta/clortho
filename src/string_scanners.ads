package String_Scanners is
   pragma SPARK_Mode;

   type Scanner_Type (<>) is private;
   type Cursor_Type is private;

   function Create (Input : String) return Scanner_Type
     with
       Pre => Input'Length < Integer'Last,
       Post => Remaining (Create'Result) = Input'Length;

   function Peek_Ahead (Scanner : Scanner_Type;
                        Amount  : Positive := 1)
                        return Character
     with
       Pre => Remaining (Scanner) - 1 >= Amount;

   function Peek_Ahead (Scanner : Scanner_Type;
                        EOF     : Character;
                        Amount  : Positive := 1)
                        return Character;

   function Remaining (Scanner : Scanner_Type) return Natural
     with
       Post => (Remaining'Result = 0) = End_Of_Input (Scanner);

   function End_Of_Input (Scanner : Scanner_Type) return Boolean;

   --  Mostly useful in contracts
   function Start_Of_Input (Scanner : Scanner_Type) return Boolean;

   function Current_Char (Scanner : Scanner_Type) return Character
     with
       Pre => not End_Of_Input (Scanner);

   procedure Next (Scanner : in out Scanner_Type;
                   Amount  : Positive := 1)
     with
       Post => (if Remaining (Scanner'Old) < Amount
                  then
                    End_Of_Input (Scanner)
                  else
                    Remaining (Scanner) = Remaining (Scanner'Old) - Amount);

   procedure Back (Scanner : in out Scanner_Type)
     with Post => (if Start_Of_Input (Scanner'Old)
                     then
                       Start_Of_Input (Scanner)
                     else
                       Remaining (Scanner) = Remaining (Scanner'Old) + 1);

   procedure Save_Position (Scanner : in out Scanner_Type)
     with
       Pre => not Position_Saved (Scanner),
       Post => Position_Saved (Scanner);

   procedure Restore_Position (Scanner : in out Scanner_Type)
     with
       Pre => Position_Saved (Scanner),
       Post => not Position_Saved (Scanner);

   procedure Forget_Position (Scanner : in out Scanner_Type)
     with
       Post => not Position_Saved (Scanner);

   --  Another function for contracts
   function Position_Saved (Scanner : Scanner_Type) return Boolean;

private
   type Cursor_Type is range 1 .. Positive'Last;

   type Scanner_Type (Length : Natural) is
      record
         Data         : String (1 .. Length);
         Cursor       : Positive := 1;
         Saved_Cursor : Positive := 1;
         Saved        : Boolean := False;
      end record
     with
       Type_Invariant =>
         Cursor - 1 <= Length
         and Length < Integer'Last
         and (if Saved then (Saved_Cursor - 1 <= Length));

end String_Scanners;
