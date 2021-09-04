pragma Ada_2012;
with Ada.Strings.Fixed;
with Ada.Text_IO;

with Interfaces.C.Strings;
with Hidden_Input.Terminal_Control;

package body Hidden_Input is
   pragma SPARK_Mode;


   ---------
   -- Get --
   ---------

   procedure Get
     (Result : out String; Last : out Natural; Marker : String := "*")
   is
      use Ada.Strings.Fixed;
      use Ada.Text_IO;

      Cursor : Positive;
      C : Character;

      Back : constant String := Marker'Length * ASCII.BS;
      Eraser : constant String := Marker'Length * ' ';
   begin
      declare
         use Terminal_Control;

         Buffer : constant Terminal_Status := Disable_Echo;
      begin
         Result := (others => ASCII.NUL);

         Cursor := Result'First;

         while Cursor <= Result'Last loop
            pragma Loop_Invariant (Cursor in Result'Range);

            Get (C);

            exit when C = ASCII.CR or C = ASCII.LF;

            case C is
            when ASCII.Bs | ASCII.DEL =>
               if Cursor > Result'First then
                  Put (Back);
                  Put (Eraser);
                  Put (Back);
                  Cursor := Cursor - 1;
               end if;

            when others =>
               Result (Cursor) := C;
               Cursor := Cursor + 1;
               Put (Marker);
            end case;
         end loop;

         Last := Cursor - 1;

         pragma Assert (Last >= Result'First - 1 and Last <= Result'Last);

         Enable_Echo (Buffer);
      end;
   end Get;

end Hidden_Input;
