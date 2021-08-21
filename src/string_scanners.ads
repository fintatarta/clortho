package String_Scanners is
   type Scanner_Type (<>) is tagged private;
   type Cursor_Type is private;

   function Create (Input : String) return Scanner_Type
     with
       Post => Remaining (Create'Result) = Input'Length;

   function Peek_Ahead (Scanner : Scanner_Type;
                        Amount  : Positive := 1)
                        return Character
     with
       Pre => Scanner.Remaining >= Amount + 1;

   function Peek_Ahead (Scanner : Scanner_Type;
                        EOF     : Character;
                        Amount  : Positive := 1)
                        return Character;

   function Remaining (Scanner : Scanner_Type) return Natural
     with
       Post => (Remaining'Result = 0) = End_Of_Input (Scanner);

   function End_Of_Input (Scanner : Scanner_Type) return Boolean;

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
                    Remaining (Scanner) = Remaining (Scanner'Old) - 1);

   function Current_Position (Scanner : Scanner_Type) return Cursor_Type;

   procedure Seek (Scanner : in out Scanner_Type;
                   Cursor  : Cursor_Type)
     with
       Post => Current_Position (Scanner) = Cursor;
private
   type Cursor_Type is range 1 .. Positive'Last;

   type Scanner_Type (Length : Natural) is tagged
      record
         Data : String (1 .. Length);
         Cursor : Positive;
      end record
     with
       Type_Invariant => Cursor <= Length + 1;

end String_Scanners;
