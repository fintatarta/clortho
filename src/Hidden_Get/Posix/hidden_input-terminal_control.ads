with Interfaces.C.Strings;

private package Hidden_Input.Terminal_Control is
   type Terminal_Status (<>) is private;

   function Disable_Echo return Terminal_Status
     with
       Import => True,
       Convention => C,
       External_Name => "disable_echo",
       Global => null;


   procedure Enable_Echo (X : Terminal_Status)
     with
       Import => True,
       Convention => C,
       External_Name => "restore_echo",
       Global => null;

private
   type Terminal_Status is new Interfaces.C.Strings.Chars_Ptr;
end Hidden_Input.Terminal_Control;
