pragma Ada_2012;
package body Password_Style_Scanner is

   --------------------------------
   -- New_Password_Style_Scanner --
   --------------------------------

   function New_Password_Style_Scanner return Scanning.Automata_Type is
      use Scanning;
   begin
      return Scanner : Automata_Type := Create do
         Add_Transition (Automata => Scanner,
                         From     => Start,
                         Input    => '/',
                         To       => Segment_Head,
                         Action   => Initialize);

         Add_Transitions (Automata       => Scanner,
                          From           => Segment_Head,
                          Edges          => (E ('!', Maybe_Complement, Prohibited_Set),
                                             E ('?', Segment_Body, Complement_Set)),
                          Default_Target => Maybe_Range,
                          Default_Action => Save_Char);


         Add_Transitions (Automata       => Scanner,
                          From           => Maybe_Complement,
                          Edges          => (1 => E ('?', Segment_Body, Complement_Set)),
                          Default_Target => Maybe_Range,
                          Default_Action => Save_Char);


         Add_Transitions (Automata       => Scanner,
                          From           => Segment_Body,
                          Edges          => (E ('-', Segment_Body, Add_Current_Char),
                                             E ('/', One_Slash, Nothing)),
                          Default_Target => Maybe_Range,
                          Default_Action => Save_Char);

         Add_Transitions (Automata       => Scanner,
                          From           => One_Slash,
                          Edges          => (1 => E ('/', Maybe_Range, Save_Char)),
                          Default_Target => Segment_Head,
                          Default_Action => Close_Set,
                          Default_Ungetc => True);


         Add_Transitions (Automata       => Scanner,
                          From           => Maybe_Range,
                          Edges          => (E ('-', After_Dash, Nothing),
                                             E ('/', One_Slash, Add_Saved)),
                          Default_Target => Maybe_Range,
                          Default_Action => Add_Saved_And_Save);

         On_End_Of_Input (Automata    => Scanner,
                          Final_State => One_Slash,
                          Action      => End_Of_Parsing);


         Default_Transition (Automata => Scanner,
                             From     => After_Dash,
                             To       => Segment_Body,
                             Action   => Add_Range);

      end return;
   end New_Password_Style_Scanner;

end Password_Style_Scanner;
