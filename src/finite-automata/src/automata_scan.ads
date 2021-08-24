with Ada.Strings.Maps;
with Text_Scanners;

generic
   type State_Type is (<>);
   type Action_Type is (<>);

   Start : State_Type;
   Error : Action_Type;
package Automata_Scan is
   pragma SPARK_Mode;

   type Automata_Type (<>) is private;

   type Running_State_Type is (Stopped, Ready, Running);

   function Create (Max_Length : Positive := 16#1_00_00#) return Automata_Type
     with
       Post => (Max_Input_Length (Create'Result) = Max_Length
                and Running_State (Create'Result) = Stopped
                and Current_State (Create'Result) = Start
                and (for all S in State_Type =>
                       (On_End_Of_Input (Create'Result, S) = Error))
                and (for all S in State_Type =>
                       (for all C in Character =>
                          not Is_Defined (Create'Result, S, C))));

   function Running_State (Automata : Automata_Type) return Running_State_Type;

   function Current_State (Automata : Automata_Type) return State_Type;

   function Max_Input_Length (Automata : Automata_Type) return Positive;

   function Is_Defined (Automata : Automata_Type;
                        From     : State_Type;
                        Input    : Character)
                        return Boolean;

   function Is_Allowed (Automata : Automata_Type;
                        From     : State_Type;
                        Input    : Character)
                        return Boolean;

   function Target  (Automata : Automata_Type;
                     From     : State_Type;
                     Input    : Character)
                     return State_Type
     with Pre => Is_Defined (Automata, From, Input);

   function Action (Automata : Automata_Type;
                    From     : State_Type;
                    Input    : Character)
                    return Action_Type
     with Pre => Is_Defined (Automata, From, Input);

   procedure Add_Transition (Automata : in out Automata_Type;
                             From     : State_Type;
                             Input    : Character;
                             To       : State_Type;
                             Action   : Action_Type;
                             Ungetc   : Boolean := False)
     with
       Pre      =>
         not Is_Defined (Automata, From, Input)
         and Running_State (Automata) = Stopped,
         Post     =>
           (
              (for all C in Character =>
                 (for all S in State_Type =>
                      (if C /= Input or S /= From then
                           (Is_Defined (Automata'Old, S, C)
                            = Is_Defined (Automata, S, C)))
                 )
              )
            and (for all S in State_Type =>
                   (On_End_Of_Input (Automata'Old, S) =
                      On_End_Of_Input (Automata, S))
                )
            and Is_Defined (Automata, From, Input)
            and Running_State (Automata) = Stopped
           )
           and then (Is_Allowed (Automata, From, Input)
                     and Automata_Scan.Action (Automata, From, Input) = Action
                     and Target (Automata, From, Input) = To
                     and Running_State (Automata) = Stopped);

   type Edge_Descriptor is private;

   function E (On_Input : Character;
               Go_To    : State_Type;
               Action   : Action_Type;
               Ungetc   : Boolean := False)
               return Edge_Descriptor;

   function E (On_Input : Ada.Strings.Maps.Character_Set;
               Go_To    : State_Type;
               Action   : Action_Type;
               Ungetc   : Boolean := False)
               return Edge_Descriptor;

   type Edge_Array is array (Positive range <>) of Edge_Descriptor;

   procedure Add_Transitions (Automata : in out Automata_Type;
                              From     : State_Type;
                              Edges    : Edge_Array);

   procedure Add_Transitions (Automata       : in out Automata_Type;
                              From           : State_Type;
                              Edges          : Edge_Array;
                              Default_Target : State_Type;
                              Default_Action : Action_Type;
                              Default_Ungetc : Boolean := False);

   procedure Add_Many_Transitions (Automata : in out Automata_Type;
                                   From     : State_Type;
                                   Input    : Ada.Strings.Maps.Character_Set;
                                   To       : State_Type;
                                   Action   : Action_Type)
     with
       Pre      =>
         (for all C of Ada.Strings.Maps.To_Sequence (Input) =>
            not Is_Defined (Automata, From, C))
         and Running_State (Automata) = Stopped,
         Post     =>
           (for all S in State_Type =>
              (On_End_Of_Input (Automata'Old, S) =
                 On_End_Of_Input (Automata, S)))
           and (for all C of Ada.Strings.Maps.To_Sequence (Input) =>
                  Is_Defined (Automata, From, C)
                and then (Is_Allowed (Automata, From, C)
                          and Automata_Scan.Action (Automata, From, C) = Action
                          and Target (Automata, From, C) = To))
           and Running_State (Automata) = Stopped;

   procedure Prohibit_Transition (Automata : in out Automata_Type;
                                  From     : State_Type;
                                  Input    : Character)
     with
       Pre      =>
         not Is_Defined (Automata, From, Input)
         and Running_State (Automata) = Stopped,
         Post     =>
           Is_Defined (Automata, From, Input)
           and not Is_Allowed (Automata, From, Input);

   procedure Default_Transition (Automata : in out Automata_Type;
                                 From     : State_Type;
                                 To       : State_Type;
                                 Action   : Action_Type;
                                 Ungetc   : Boolean := False)
     with
       Pre =>
         Running_State (Automata) = Stopped,
         Post =>
           Running_State (Automata) = Stopped
           and (for all S in State_Type =>
                  (On_End_Of_Input (Automata'Old, S) =
                     On_End_Of_Input (Automata, S)))
           and (for all C in Character =>
                  (Is_Defined (Automata, From, C)
                   and then
                     (if not Is_Defined (Automata'Old, From, C) then
                          (Automata_Scan.Action (Automata, From, C) = Action
                           and Target (Automata, From, C) = To))));

   function On_End_Of_Input (Automata : Automata_Type;
                             State    : State_Type)
                             return Action_Type;
   procedure On_End_Of_Input  (Automata    : in out Automata_Type;
                               Final_State : State_Type;
                               Action      : Action_Type)
     with
       Pre =>
         Action /= Error
         and On_End_Of_Input (Automata, Final_State) = Error
         and Running_State (Automata) = Stopped,
         Post =>
           On_End_Of_Input (Automata, Final_State) = Action
           and Running_State (Automata) = Stopped
           and (for all C in Character =>
                  (for all S in State_Type =>
                     (Is_Defined (Automata'Old, S, C)
                      = Is_Defined (Automata, S, C))));

   procedure Reset (Automata : in out Automata_Type;
                    Input    : String)
     with
       Pre => Input'Length <= Max_Input_Length (Automata),
       Post =>
         Current_State (Automata) = Start
         and Running_State (Automata) = Ready;

   procedure Next (Automata : in out Automata_Type;
                   Action   :    out Action_Type;
                   Char     :    out Character)
     with
       Pre =>
         Running_State (Automata) /= Stopped,
         Post =>
           (Running_State (Automata) = Running
            or Running_State (Automata) = Stopped)
           and (if Action = Error then Running_State (Automata) = Stopped);

private

   type Edge_Descriptor is
      record
         On_Input : Ada.Strings.Maps.Character_Set;
         Target   : State_Type;
         Action   : Action_Type;
         Ungetc   : Boolean;
      end record;

   type Transition_State is (Undefined, Prohibited, Allowed);

   type Transition_Entry is
      record
         Target : State_Type;
         Action : Action_Type;
         State  : Transition_State := Undefined;
         Ungetc : Boolean;
      end record;

   type Transition_Matrix is array (State_Type, Character) of Transition_Entry;

   type End_Of_Input_Vector is array (State_Type) of Action_Type;

   type Automata_Type (Max_Length : Positive) is
      record
         Current_State   : State_Type := Start;
         Transitions     : Transition_Matrix;
         On_End_Of_Input : End_Of_Input_Vector := (others => Error);
         Running_State   : Running_State_Type;
         Reader          : Text_Scanners.Input_Reader (Max_Length);
      end record;
end Automata_Scan;
