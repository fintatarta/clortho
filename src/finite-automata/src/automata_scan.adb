pragma Ada_2012;

package body Automata_Scan is
   pragma SPARK_Mode;
   ------------
   -- Create --
   ------------

   function Create (Max_Length : Positive := 16#1_00_00#)
                    return Automata_Type
   is
      use Text_Scanners;

      Bogus_Transition : constant Transition_Entry := (Target => Start,
                                                       Action => Error,
                                                       Ungetc => False,
                                                       State  => Undefined);
   begin
      return (Max_Length      => Max_Length,
              Reader          => New_Scanner (Max_Length),
              Current_State   => Start,
              Running_State   => Stopped,
              On_End_Of_Input => (others => Error),
              Transitions     => (others => (others => Bogus_Transition)));
   end Create;

   function Running_State (Automata : Automata_Type) return Running_State_Type
   is (Automata.Running_State);

   function Max_Input_Length (Automata : Automata_Type) return Positive
   is (Automata.Max_Length);

   -------------------
   -- Current_State --
   -------------------

   function Current_State (Automata : Automata_Type) return State_Type
   is (Automata.Current_State);

   ----------------
   -- Is_Defined --
   ----------------

   function Is_Defined
     (Automata : Automata_Type; From : State_Type; Input : Character)
      return Boolean
   is (Automata.Transitions (From, Input).State /= Undefined);

   ----------------
   -- Is_Allowed --
   ----------------

   function Is_Allowed
     (Automata : Automata_Type; From : State_Type; Input : Character)
      return Boolean
   is (Automata.Transitions (From, Input).State = Allowed);

   ------------
   -- Target --
   ------------

   function Target
     (Automata : Automata_Type; From : State_Type; Input : Character)
      return State_Type
   is (Automata.Transitions (From, Input).Target);

   ------------
   -- Action --
   ------------

   function Action
     (Automata : Automata_Type; From : State_Type; Input : Character)
      return Action_Type
   is (Automata.Transitions (From, Input).Action);

   -------
   -- E --
   -------

   function E (On_Input : Character;
               Go_To    : State_Type;
               Action   : Action_Type;
               Ungetc   : Boolean := False)
               return Edge_Descriptor
   is (Edge_Descriptor'(On_Input => Ada.Strings.Maps.To_Set (On_Input),
                        Target   => Go_To,
                        Action   => Action,
                        Ungetc   => Ungetc));

   -------
   -- E --
   -------

   function E (On_Input : Ada.Strings.Maps.Character_Set;
               Go_To    : State_Type;
               Action   : Action_Type;
               Ungetc   : Boolean := False)
               return Edge_Descriptor
   is (Edge_Descriptor'(On_Input => On_Input,
                        Target   => Go_To,
                        Action   => Action,
                        Ungetc   => Ungetc));

   ---------------------
   -- Add_Transitions --
   ---------------------

   procedure Add_Transitions (Automata : in out Automata_Type;
                              From     : State_Type;
                              Edges    : Edge_Array)
   is
   begin
      for Edge of Edges loop
         Add_Many_Transitions (Automata => Automata,
                               From     => From,
                               Input    => Edge.On_Input,
                               To       => Edge.Target,
                               Action   => Edge.Action);
      end loop;
   end Add_Transitions;

   ---------------------
   -- Add_Transitions --
   ---------------------

   procedure Add_Transitions (Automata       : in out Automata_Type;
                              From           : State_Type;
                              Edges          : Edge_Array;
                              Default_Target : State_Type;
                              Default_Action : Action_Type;
                              Default_Ungetc : Boolean := False)
   is
   begin
      Add_Transitions (Automata       => Automata,
                       From           => From,
                       Edges          => Edges);

      Default_Transition (Automata => Automata,
                          From     => From,
                          To       => Default_Target,
                          Action   => Default_Action,
                          Ungetc   => Default_Ungetc);
   end Add_Transitions;

   --------------------
   -- Add_Transition --
   --------------------

   procedure Add_Transition
     (Automata : in out Automata_Type;
      From     : State_Type;
      Input    : Character;
      To       : State_Type;
      Action   : Action_Type;
      Ungetc   : Boolean := False)
   is
   begin
      if Is_Defined (Automata, From, Input) then
         raise Constraint_Error;
      end if;

      Automata.Transitions (From, Input) := (Target => To,
                                             Action => Action,
                                             State  => Allowed,
                                             Ungetc => Ungetc);
   end Add_Transition;

   --------------------
   -- Add_Transition --
   --------------------

   procedure Add_Many_Transitions
     (Automata : in out Automata_Type; From : State_Type;
      Input    :        Ada.Strings.Maps.Character_Set; To : State_Type;
      Action   :        Action_Type)
   is
      S : constant String := Ada.Strings.Maps.To_Sequence (Input);
   begin
      for I in S'Range  loop
         Add_Transition (Automata, From, S (I), To, Action);

         pragma Loop_Invariant ((for all K in S'First .. I =>
                                   Is_Allowed (Automata, From, S (K))));
      end loop;
   end Add_Many_Transitions;

   -------------------------
   -- Prohibit_Transition --
   -------------------------

   procedure Prohibit_Transition
     (Automata : in out Automata_Type; From : State_Type; Input : Character)
   is
   begin
      if Is_Defined (Automata, From, Input) then
         raise Constraint_Error;
      end if;

      Automata.Transitions (From, Input).State := Prohibited;
   end Prohibit_Transition;

   ------------------------
   -- Default_Transition --
   ------------------------

   procedure Default_Transition
     (Automata : in out Automata_Type;
      From     : State_Type;
      To       : State_Type;
      Action   : Action_Type;
      Ungetc   : Boolean := False)
   is
      Old : constant Automata_Type := Automata;
   begin
      for C in Character loop
         pragma Loop_Invariant
           (for all K in C .. Character'Last =>
              Is_Defined (Old, From, K) = Is_Defined (Automata, From, K));

         pragma Loop_Invariant
           (if C > Character'First then
              (for all K in Character'First .. Character'Pred (C) =>
                   Is_Defined (Automata, From, K))
            and
              (for all K in Character'First .. Character'Pred (C) =>
                   (if not Is_Defined (Old, From, K) then
                      (Automata_Scan.Action (Automata, From, K) = Action
                       and Target (Automata, From, K) = To))));

         pragma Loop_Invariant (Running_State (Automata) = Stopped);

         pragma Loop_Invariant
           (for all S in State_Type =>
              On_End_Of_Input (Old, S) = On_End_Of_Input (Automata, S));

         if not Is_Defined (Automata, From, C) then
            Add_Transition (Automata, From, C, To, Action, Ungetc);

            pragma Assert (not Is_Defined (Old, From, C)
                           and Automata_Scan.Action (Automata, From, C) = Action
                           and Target (Automata, From, C) = To);
         end if;

         pragma Assert (Is_Defined (Automata, From, C));

      end loop;
   end Default_Transition;

   function On_End_Of_Input (Automata : Automata_Type;
                             State    : State_Type)
                             return Action_Type
   is (Automata.On_End_Of_Input (State));

   ---------------------
   -- On_End_Of_Input --
   ---------------------

   procedure On_End_Of_Input  (Automata    : in out Automata_Type;
                               Final_State : State_Type;
                               Action      : Action_Type)
   is
   begin
      if On_End_Of_Input (Automata, Final_State) /= Error or Action = Error then
         raise Constraint_Error;
      end if;

      Automata.On_End_Of_Input (Final_State) := Action;
   end On_End_Of_Input;

   -----------
   -- Reset --
   -----------

   procedure Reset (Automata : in out Automata_Type;
                    Input    : String)
   is
   begin
      if Input'Length > Automata.Max_Length then
         raise Constraint_Error;
      end if;

      Automata.Current_State := Start;
      Text_Scanners.Reset (Automata.Reader, Input);
      Automata.Running_State := Ready;
   end Reset;

   ----------
   -- Next --
   ----------

   procedure Next
     (Automata : in out Automata_Type;
      Action   :    out Action_Type;
      Char     :    out Character)
   is
      use Text_Scanners;
   begin
      --  Put_Line ("**" & Automata.Current_State'Image);
      pragma Assert (not Is_Current_Char_Unused (Automata.Reader));

      Next_Character (Automata.Reader);

      if End_Of_Input (Automata.Reader) then
         Action := Automata.On_End_Of_Input (Automata.Current_State);
         Char := ASCII.NUL;
         Automata.Running_State := Stopped;
         return;
      end if;

      Automata.Running_State := Running;

      Current_Char (Automata.Reader, Char);

      if not Is_Allowed (Automata, Automata.Current_State, Char) then
         Action := Error;
         Automata.Running_State := Stopped;
         return;
      end if;

      declare
         To_Do : constant Transition_Entry :=
                   Automata.Transitions (Automata.Current_State, Char);
      begin
         Action := To_Do.Action;

         Automata.Current_State := To_Do.Target;

         if To_Do.Ungetc then
            if Begin_Of_Input (Automata.Reader) then
               raise Constraint_Error;
            end if;

            Char := ASCII.NUL;
            Ungetc (Automata.Reader);
         end if;

         if Action = Error then
            Automata.Running_State := Stopped;
         end if;
      end;
   end Next;

   function Is_Current_Char_Unused (Automata : Automata_Type) return Boolean
   is (Text_Scanners.Is_Current_Char_Unused (Automata.Reader));

   function End_Of_Input (Automata : Automata_Type) return Boolean
   is (Text_Scanners.End_Of_Input (Automata.Reader));
   pragma Unreferenced (End_Of_Input);

   function Begin_Of_Input (Automata : Automata_Type) return Boolean
   is (Text_Scanners.Begin_Of_Input (Automata.Reader));
   pragma Unreferenced (Begin_Of_Input);

end Automata_Scan;
