with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;

package Clortho.Exit_Statuses is
   pragma SPARK_Mode;

   type Exit_Status is private;

   function Success return Exit_Status
     with
       Post => Is_Success (Success'Result);

   function Failure (Why : String) return Exit_Status
     with
       Post =>
         not Is_Success (Failure'Result)
         and then Reason (Failure'Result) = Why;

   function Is_Success (Item : Exit_Status) return Boolean;

   function Reason (Item : Exit_Status) return String
     with
       Pre => not Is_Success (Item);

   type String_Status is
      record
         S      : Unbounded_String;
         Status : Exit_Status;
      end record;
private
   type Exit_Status is
      record
         Success : Boolean;
         Reason  : Unbounded_String;
      end record;

end Clortho.Exit_Statuses;
