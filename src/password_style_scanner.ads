with Automata_Scan;

package Password_Style_Scanner is
 type State_Type is
     (
      Start,
      Segment_Head,
      Maybe_Complement,
      Segment_Body,
      Maybe_Range,
      One_Slash,
      After_Dash
     );

   type Action_Type is
     (
      Nothing,
      Error,
      End_Of_Parsing,
      Initialize,
      Save_Char,
      Add_Saved,
      Add_Current_Char,
      Add_Saved_And_Save,
      Add_Range,
      Prohibited_Set,
      Complement_Set,
      Close_Set
     );

   package Scanning is
     new Automata_Scan (State_Type  => State_Type,
                        Action_Type => Action_Type,
                        Start       => Start,
                        Error       => Error);

   function New_Password_Style_Scanner return Scanning.Automata_Type;
end Password_Style_Scanner;
